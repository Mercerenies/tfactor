{-# LANGUAGE FlexibleContexts, ViewPatterns, KindSignatures, RankNTypes, TypeFamilies, ScopedTypeVariables #-}

module Factor.State(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    Module(Module), AliasDecl(..),
                    readerModule, readerNames, readerResources,
                    moduleNames, moduleAliases, moduleIsType,
                    newReader, emptyModule,
                    declsToReadOnly, atQIdResource, atQId, lookupFn, lookupFnName,
                    allNamesInModule, allNames,
                    readerFunctionType, readerMacroType,
                    merge, mapToReader) where

import Factor.Error
import Factor.Code
import Factor.Id
import Factor.Type
import Factor.Names
import Factor.State.Types
import Factor.State.Resource

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import qualified Data.List as List
import Data.Foldable
import Data.Maybe
import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Monad.State
import Control.Lens

declsToReadOnly :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
                   QId -> [Declaration] -> Module -> m Module
declsToReadOnly qid ds r = foldM go r ds
    where go reader decl =
              case decl of
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def)
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineFunction qid' v t def) reader
                MacroDecl t (Macro v def)
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineMacro qid' v t def) reader
                ModuleDecl v def
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          let qid' = qid <> QId [v]
                          inner <- declsToReadOnly qid' def emptyModule
                          traverseOf moduleNames (defineModule qid' v inner) reader
                ModuleSyn v dest
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineResource qid' v (ModuleSynonym dest)) reader
                RecordDecl v def
                 | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          let qid' = qid <> QId [v]
                          inner <- expandRecordDecl qid' def emptyModule
                          let inner' = set moduleIsType True inner
                          traverseOf moduleNames (defineModule qid' v inner') reader
                AliasDecl i j -> pure $ over moduleAliases (++ [Alias i j]) reader
                OpenDecl j -> pure $ over moduleAliases (++ [Open j]) reader

defineFunction :: MonadState (ResourceTable ReaderValue) m =>
                  QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineFunction q v t def = defineResource q v (UDFunction t $ Function (Just v) def)

defineMacro :: MonadState (ResourceTable ReaderValue) m =>
               QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineMacro q v t def = defineResource q v (UDMacro t $ Macro v def)

defineModule :: MonadState (ResourceTable ReaderValue) m =>
                QId -> Id -> Module -> Map Id RId -> m (Map Id RId)
defineModule q v def = defineResource q v (ModuleValue def)

-- TODO Mark which modules can be treated as types and which cannot,
-- as a flag on the Module type itself.

-- Hopefully, somewhere down the road, record declarations will be
-- able to be a declaration macro defined in Prelude, and then they'll
-- just translate inside the language. But for now, it's special
-- syntax sugar since the tools to do that aren't in place yet.
expandRecordDecl :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
                    QId -> [RecordInfo] -> Module -> m Module
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
                RecordField i t ->
                    let var = unusedTypeVar
                        fntype = polyFunctionType [var] [NamedType qid] (RestQuant var)
                                                        [t] (RestQuant var)
                        -- We're iterating over the exact same data
                        -- used to construct collectedFields, so the
                        -- field must exist.
                        indx = maybe (error "Internal error in expandRecordDecl") id
                                (List.findIndex (\(i', _) -> i == i') collectedFields)
                        impl = Sequence [
                                Literal (Int . toInteger $ indx),
                                Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-get"]), -- TODO Put this name in Factor.Names
                                Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
                               ]
                        d = FunctionDecl fntype (Function (Just i) impl)
                    in declsToReadOnly qid [d] reader
                RecordOrdinaryDecl d -> declsToReadOnly qid [d] reader

-- TODO Is this whole thing valid from a Traversal law standpoint...?

atRId :: RId -> Traversal' ReadOnlyState ReaderValue
atRId rid = readerResources . ix rid

moduleHelper :: Traversal' ReaderValue Module
moduleHelper f (ModuleValue m) = ModuleValue <$> f m
moduleHelper _ x = pure x

atQIdResource0 :: forall f. Applicative f => QId -> (RId -> f RId) -> ReadOnlyState -> f ReadOnlyState
atQIdResource0 (QId xs0) f r0 = go xs0 readerNames
    where go :: [Id] -> Traversal' ReadOnlyState (Map Id RId) -> f ReadOnlyState
          go [] _ = pure r0
          go [x] r = (r . ix x) f r0
          go (x:xs) r = case Map.lookup x (r0^.r) of
                          Nothing -> pure r0
                          Just rid -> go xs (readerResources . ix rid . moduleHelper . moduleNames)

-- So for whatever reason, Haskell needed some help (in the form of
-- ScopedTypeVariables) getting the type signatures of atQIdResource0
-- right. So the type signature up there is messy, and this function
-- acts simply as an assertion that it's still the traversal that I
-- want.
atQIdResource :: QId -> Traversal' ReadOnlyState RId
atQIdResource = atQIdResource0

atQId :: QId -> Traversal' ReadOnlyState ReaderValue
atQId (QId xs0) f r0 = go xs0 (r0^.readerNames)
    where go [] _ = pure r0
          go [x] r = case Map.lookup x r of
                       Nothing -> pure r0
                       Just rid -> atRId rid f r0
          go (x:xs) r = case Map.lookup x r >>= flip getResource (r0^.readerResources) of
                          Nothing -> pure r0
                          Just (ModuleValue m) -> go xs (m^.moduleNames)
                          Just _ -> pure r0

-- TODO This is used for more than just functions. Change its name to
-- reflect that, and make it stop throwing NoSuchFunction, since that
-- error may not be right.
lookupFn :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn (QId ids) reader =
  let go (ModuleValue names) i =
          case (names^.moduleNames.at i) >>= flip getResource (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> pure x
      go _ _ = throwError $ NoSuchModule (QId ids)
  in foldM go (ModuleValue $ view readerModule reader) ids

lookupFnName :: MonadError FactorError m => QId -> ReadOnlyState -> m QId
lookupFnName (QId ids) reader =
  let go (ModuleValue names) [i] =
          case (names^.moduleNames.at i) >>= flip getResourceName (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> pure x
      go (ModuleValue names) (i:is) =
          case (names^.moduleNames.at i) >>= flip getResource (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> go x is
      go _ _ = throwError $ NoSuchModule (QId ids)
  in go (ModuleValue $ view readerModule reader) ids

allNamesInModule :: ResourceTable ReaderValue -> QId -> Module -> [QId]
allNamesInModule resources k0 = fold . Map.mapWithKey go' . view moduleNames
    where go :: Id -> ReaderValue -> [QId]
          go k1 v =
              let k = k0 <> QId [k1]
                  innernames = case v of
                                 UDFunction {} -> []
                                 BIFunction {} -> []
                                 UDMacro {} -> []
                                 ModuleValue m' -> allNamesInModule resources k m'
                                 ModuleSynonym {} -> [] -- Can't look them up yet, as the
              in k : innernames                         -- synonym hasn't been resolved.
          go' :: Id -> RId -> [QId]
          go' k v = case getResource v resources of
                      Nothing -> []
                      Just v' -> go k v'

allNames :: ReadOnlyState -> [QId]
allNames reader = allNamesInModule (reader^.readerResources) (QId []) (reader^.readerModule)

-- TODO Internally combine these into one function, since they're
-- mutually exclusive, then provide the same public API in a more
-- consolidated way.

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType (UDFunction t _) = Just t
readerFunctionType (BIFunction t _) = Just t
readerFunctionType (UDMacro _ _) = Nothing
readerFunctionType (ModuleValue _) = Nothing
readerFunctionType (ModuleSynonym _) = Nothing

readerMacroType :: ReaderValue -> Maybe PolyFunctionType
readerMacroType (UDFunction _ _) = Nothing
readerMacroType (BIFunction _ _) = Nothing
readerMacroType (UDMacro t _) = Just t
readerMacroType (ModuleValue _) = Nothing
readerMacroType (ModuleSynonym _) = Nothing

merge :: MonadError FactorError m => ReadOnlyState -> ReadOnlyState -> m ReadOnlyState
merge (ReadOnlyState (Module m a t) r) (ReadOnlyState (Module m' a' t') r') =
    ReadOnlyState <$> (Module <$> merged <*> pure (a ++ a') <*> pure (t || t')) <*> pure rtable
        where failure = Merge.zipWithAMatched $ \k _ _ -> throwError (DuplicateDecl k)
              renamedm = fmap (+ resourceCount r) m'
              renamedr = modifyRIds (+ resourceCount r) r'
              merged = Merge.mergeA Merge.preserveMissing Merge.preserveMissing failure m renamedm
              rtable = r `catResources` renamedr

mapToReader :: Map Id ReaderValue -> ReadOnlyState
mapToReader m = ReadOnlyState (Module modl [] False) resources
    where (modl, resources) = runState (Map.traverseWithKey go m) newResourceTable
          go k v = appendResource' (QId [k]) v
