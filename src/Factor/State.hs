{-# LANGUAGE FlexibleContexts, ViewPatterns, RankNTypes,
  ConstraintKinds, KindSignatures, TemplateHaskell #-}

module Factor.State(EvalState(..), ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    Module(Module), AliasDecl(..), BuiltIn(..), BuiltInConstraints,
                    readerModule, readerNames, moduleNames, moduleAliases,
                    newState, newReader, emptyModule,
                    pushStack, peekStackMaybe, peekStack, popStackMaybe, popStack,
                    declsToReadOnly, atQId, lookupFn,
                    allNamesInModule, allNames,
                    readerFunctionType, readerMacroType,
                    merge) where

import Factor.Error
import Factor.Code
import Factor.Id
import Factor.Type
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack
import Factor.Names

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import Data.Foldable
import Data.Maybe
import Control.Monad.Reader hiding (reader)
import Control.Monad.State
import Control.Monad.Except
import Control.Lens

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show, Eq)

data ReadOnlyState = ReadOnlyState {
      _readerModule :: Module
    }

data ReaderValue = UDFunction PolyFunctionType  Function    -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function
                 | UDMacro PolyFunctionType  Macro       -- User-defined macro
                 | ModuleValue Module

data Module = Module {
      _moduleNames :: Map Id ReaderValue,
      _moduleAliases :: [AliasDecl]
    }

data AliasDecl = Alias Id QId
               | Open QId
                 deriving (Show, Eq)

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

makeLenses ''ReadOnlyState
makeLenses ''Module

readerNames :: Lens' ReadOnlyState (Map Id ReaderValue)
readerNames = readerModule . moduleNames

newState :: EvalState
newState = EvalState Stack.empty

newReader :: ReadOnlyState
newReader = ReadOnlyState emptyModule

emptyModule :: Module
emptyModule = Module Map.empty []

pushStack :: MonadState EvalState m => Stack Data -> m ()
pushStack xs = modify $ \s -> s { stateStack = Stack.appendStack xs (stateStack s) }

peekStackMaybe :: MonadState EvalState m => Int -> m (Maybe (Stack Data))
peekStackMaybe n = go <$> gets stateStack
    where go = fmap fst . Stack.splitStack n

peekStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m (Stack Data)
peekStack n = peekStackMaybe n >>= maybe (throwError StackUnderflow) return

popStackMaybe :: MonadState EvalState m => Int -> m (Maybe (Stack Data))
popStackMaybe n = gets stateStack >>= go
    where go ss = case Stack.splitStack n ss of
                    Nothing -> pure Nothing
                    Just (a, b) -> Just a <$ modify (\s -> s { stateStack = b })

popStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m (Stack Data)
popStack n = popStackMaybe n >>= maybe (throwError StackUnderflow) return

declsToReadOnly :: MonadError FactorError m =>
                   QId -> [Declaration] -> Module -> m Module
declsToReadOnly qid ds r = foldM go r ds
    where go reader decl =
              case decl of
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def)
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ over moduleNames (defineFunction v t def) reader
                MacroDecl t (Macro v def)
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ over moduleNames (defineMacro v t def) reader
                ModuleDecl v def
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          let qid' = qid <> QId [v]
                          inner <- declsToReadOnly qid' def emptyModule
                          pure $ over moduleNames (defineModule v inner) reader
                RecordDecl v def
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          let qid' = qid <> QId [v]
                          inner <- expandRecordDecl qid' def emptyModule
                          pure $ over moduleNames (defineModule v inner) reader
                AliasDecl i j -> pure $ over moduleAliases (++ [Alias i j]) reader
                OpenDecl j -> pure $ over moduleAliases (++ [Open j]) reader

defineFunction :: Id -> PolyFunctionType -> Sequence -> Map Id ReaderValue -> Map Id ReaderValue
defineFunction v t def = Map.insert v (UDFunction t $ Function (Just v) def)

defineMacro :: Id -> PolyFunctionType -> Sequence -> Map Id ReaderValue -> Map Id ReaderValue
defineMacro v t def = Map.insert v (UDMacro t $ Macro v def)

defineModule :: Id -> Module -> Map Id ReaderValue -> Map Id ReaderValue
defineModule v def = Map.insert v (ModuleValue def)

-- TODO Mark which modules can be treated as types and which cannot,
-- as a flag on the Module type itself.

-- Hopefully, somewhere down the road, record declarations will be
-- able to be a declaration macro defined in Prelude, and then they'll
-- just translate inside the language. But for now, it's special
-- syntax sugar since the tools to do that aren't in place yet.
expandRecordDecl :: MonadError FactorError m => QId -> [RecordInfo] -> Module -> m Module
expandRecordDecl qid ds r = foldM go r ds
    where collectField (RecordField i t) = Just (i, t)
          collectField _ = Nothing
          collectedFields = mapMaybe collectField ds
          usedTypeVars = concatMap (\(_, t) -> allQuantVars t) collectedFields
          unusedTypeVar = freshVar "R" usedTypeVars
          go reader decl =
              case decl of
                RecordConstructor v ->
                    let var = unusedTypeVar
                        args = reverse $ fmap (\(_, t) -> t) collectedFields
                        fntype = polyFunctionType [var] args (RestQuant var)
                                                  [NamedType qid] (RestQuant var)
                        impl = Sequence [
                                Literal (Int . toInteger $ length collectedFields),
                                Literal (String $ qidName qid),
                                Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-construct"]), -- TODO Put this name in Factor.Names
                                Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
                               ]
                        d = FunctionDecl fntype (Function (Just v) impl)
                    in declsToReadOnly qid [d] reader
                RecordField _i _t -> pure reader -- error "TODO This"
                RecordOrdinaryDecl d -> declsToReadOnly qid [d] reader

atQId :: QId -> Traversal' ReadOnlyState (Maybe ReaderValue)
atQId (QId xs0) = readerNames . go xs0
    where go :: [Id] -> Traversal' (Map Id ReaderValue) (Maybe ReaderValue)
          go [] = error "Empty identifier in atQId"
          go [x] = at x
          go (x:xs) = at x . shim . go xs
          -- TODO This follows the first Traversal law. Does it follow
          -- the second? I think so but I'm not 100% certain.
          shim _ Nothing = pure Nothing
          shim f (Just (ModuleValue m)) = Just . ModuleValue <$> traverseOf moduleNames f m
          shim _ (Just x) = pure $ Just x

lookupFn :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn (QId ids) reader =
  -- Lookup the top-level name in the alias table first.
  let go (ModuleValue names) i =
          maybe (throwError $ NoSuchFunction (QId ids)) pure $ names^.moduleNames.at i
      go _ _ = throwError $ NoSuchModule (QId ids)
  in foldM go (ModuleValue $ view readerModule reader) ids

allNamesInModule :: QId -> Module -> [QId]
allNamesInModule k0 = fold . Map.mapWithKey go . view moduleNames
    where go k1 v =
              let k = k0 <> QId [k1]
                  innernames = case v of
                                 UDFunction {} -> []
                                 BIFunction {} -> []
                                 UDMacro {} -> []
                                 ModuleValue m' -> allNamesInModule k m'
              in k : innernames

allNames :: ReadOnlyState -> [QId]
allNames (view readerModule -> m) = allNamesInModule (QId []) m

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType (UDFunction t _) = Just t
readerFunctionType (BIFunction t _) = Just t
readerFunctionType (UDMacro _ _) = Nothing
readerFunctionType (ModuleValue _) = Nothing

readerMacroType :: ReaderValue -> Maybe PolyFunctionType
readerMacroType (UDFunction _ _) = Nothing
readerMacroType (BIFunction _ _) = Nothing
readerMacroType (UDMacro t _) = Just t
readerMacroType (ModuleValue _) = Nothing

merge :: MonadError FactorError m => ReadOnlyState -> ReadOnlyState -> m ReadOnlyState
merge (ReadOnlyState (Module m a)) (ReadOnlyState (Module m' a')) =
    fmap ReadOnlyState (Module <$> merged <*> pure (a ++ a'))
        where failure = Merge.zipWithAMatched $ \k _ _ -> throwError (DuplicateDecl k)
              merged = Merge.mergeA Merge.preserveMissing Merge.preserveMissing failure m m'
