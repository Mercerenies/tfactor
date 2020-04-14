{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Factor.Type.Checker where

import Factor.Code
import Factor.State
import Factor.Type
import Factor.Type.Unify
import Factor.Error
import Factor.Id

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Lens

data TypeCheckerPass = MacroPass | FunctionPass
                       deriving (Show, Read, Eq, Ord)

monomorphize :: MonadState (Set Id) m => PolyFunctionType -> m FunctionType
monomorphize (PolyFunctionType ids fn) = do
  knowns <- get
  let isConflicting v = v `elem` ids && v `elem` knowns
  return $ renameToAvoidConflicts' isConflicting fn

monomorphize' :: Set Id -> PolyFunctionType -> FunctionType
monomorphize' v p = evalState (monomorphize p) v

knownVars :: MonadState (Set Id) m => [Id] -> m ()
knownVars vs = modify $ (Set.fromList vs `Set.union`)

capture :: MonadWriter w m => m a -> m (a, w)
capture = censor (const mempty) . listen

typeOfValue :: (MonadError FactorError m, MonadReader ReadOnlyState m,
                MonadWriter AssumptionsAll m) =>
               TypeCheckerPass -> Data -> StateT (Set Id) m Type
typeOfValue tpass value = case value of
                             Int _ -> return (PrimType TInt)
                             Bool _ -> return (PrimType TBool)
                             String _ -> return (PrimType TString)
                             Symbol _ -> return (PrimType TSymbol)
                             RecordInstance v _ -> return (NamedType v)
                             FunctionValue (Function _ ss) -> do
                                (PolyFunctionType ids ss', AssumptionsAll w w') <-
                                   capture (typeOfSeq tpass ss)
                                let (univ , assum ) = Map.partitionWithKey (\k _ -> k `elem` ids) w
                                let (univ', assum') = Map.partitionWithKey (\k _ -> k `elem` ids) w'
                                tell $ AssumptionsAll assum assum'
                                let asm = AssumptionsAll univ univ'
                                casm <- consolidateUntilDone asm
                                let ss'' = case substituteFully casm (FunType ss') of
                                             FunType x -> x
                                             _ -> error "Substitution changed type in typeOfValue"
                                ss''' <- monomorphize (PolyFunctionType ids ss'')
                                return $ FunType ss'''

-- A single statement always carries an effect on the stack, hence we
-- treat its type as a function type.
typeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
          TypeCheckerPass -> Statement -> StateT (Set Id) m PolyFunctionType
typeOf tpass stmt = case stmt of
                      Call v -> do
                                fn <- ask >>= lookupFn v
                                case tpass of
                                  FunctionPass ->
                                           case readerFunctionType fn of
                                             Just x -> return x
                                             Nothing -> throwError NotAFunction
                                  MacroPass ->
                                           case readerMacroType fn of
                                             Just x -> return x
                                             Nothing -> typeOf tpass (Literal $ Symbol (qidName v))
                      Literal d -> do
                                d' <- typeOfValue tpass d
                                let quants = allQuantVars d'
                                    r' = head [v | n <- [0 :: Int ..]
                                                 , let v = Id "R" <> Id (show n)
                                                 , not (v `elem` quants)]
                                    fn = functionType [] (RestQuant r') [d'] (RestQuant r')
                                return $ PolyFunctionType (allQuantVars $ FunType fn) fn

typeOfSeq :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
             TypeCheckerPass -> Sequence -> StateT (Set Id) m PolyFunctionType
typeOfSeq tpass (Sequence xs) = foldM go emptyPolyFnType xs
    where go acc z = do
            knownVars $ quantifiedVars acc
            z' <- typeOf tpass z
            knownVars $ quantifiedVars z'
            res <- composePFunctions acc z'
            knownVars $ quantifiedVars res
            return res

-- A weaker typecheck for when we don't know what we're looking for
-- but just want to catch any glaring errors that cause
-- contradictions.
checkHasDefinedType :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                       TypeCheckerPass -> Sequence -> m ()
checkHasDefinedType tpass ss = runWriterT go >>= doSub
    where go = evalStateT (typeOfSeq tpass ss) mempty
          doSub (PolyFunctionType {}, w) = do
            Assumptions _ _ <- consolidateUntilDone w
            pure ()

-- When checking the declared type of a thing, all we care about is
-- that the types line up. The assumptions we used to get there are
-- irrelevant.
checkDeclaredType :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                     TypeCheckerPass -> PolyFunctionType -> Sequence -> m ()
checkDeclaredType tpass (PolyFunctionType ids declared) ss = runWriterT go >>= doSub
    where go = do
            ss' <- evalStateT (typeOfSeq tpass ss) mempty
            let inferred = underlyingFnType ss'
            inferred `isFnSubtypeOf` toGround' ids declared
          toGround' i fn = case toGround i (FunType fn) of
                             FunType fn' -> fn'
                             _ -> error "toGround changed shape of type in checkDeclaredType"
          doSub ((), w) = do
                 Assumptions _ _ <- consolidateUntilDone w
                 pure ()

-- TODO We make up some variables here. It's safe right now, but once
-- modules start taking type arguments, it will cease to be safe.
checkTypeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
               TypeCheckerPass -> ReaderValue -> m ()
checkTypeOf tpass (UDFunction t (Function _ ss)) =
    case tpass of
      FunctionPass -> checkDeclaredType tpass t ss
      MacroPass ->
          -- "Declared" type is ( 'R -- ''S ) with no quantifiers.
          -- This means that the 'R can't be unified to anything, but
          -- ''S can, so our function must be able to expand macros
          -- starting from an empty stack.
          let t0 = polyFunctionType [] [] (RestGround (Id "R")) [] (RestQuant (Id "S"))
          in checkDeclaredType tpass t0 ss
checkTypeOf _ (BIFunction _ _) = pure () -- We don't typecheck primitives.
checkTypeOf tpass (UDMacro t (Macro _ ss)) =
    case tpass of
      FunctionPass -> checkDeclaredType tpass t ss
      MacroPass ->
          -- "Declared" type is ( 'R -- ''S ) with no quantifiers.
          -- This means that the 'R can't be unified to anything, but
          -- ''S can, so our function must be able to expand macros
          -- starting from an empty stack.
          let t0 = polyFunctionType [] [] (RestGround (Id "R")) [] (RestQuant (Id "S"))
          in checkDeclaredType tpass t0 ss
checkTypeOf tpass (ModuleValue m) = checkTypes tpass (view moduleNames m) -- Nothing to do with the module as a whole (yet).

checkTypes :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
              TypeCheckerPass -> Map Id ReaderValue -> m ()
checkTypes tpass = void . traverse (checkTypeOf tpass)

checkAllTypes :: (MonadError FactorError m, MonadReader ReadOnlyState m) => TypeCheckerPass -> m ()
checkAllTypes tpass = asks (view readerNames) >>= checkTypes tpass
