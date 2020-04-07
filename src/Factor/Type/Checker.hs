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
               Data -> StateT (Set Id) m Type
typeOfValue value = case value of
                      Int _ -> return (PrimType TInt)
                      Bool _ -> return (PrimType TBool)
                      String _ -> return (PrimType TString)
                      FunctionValue (Function _ ss) -> do
                         (PolyFunctionType ids ss', AssumptionsAll w w') <- capture (typeOfSeq ss)
                         let (univ , assum ) = Map.partitionWithKey (\k _ -> k `elem` ids) w
                         let (univ', assum') = Map.partitionWithKey (\k _ -> k `elem` ids) w'
                         tell $ AssumptionsAll assum assum'
                         let asm = AssumptionsAll univ univ'
                         casm <- consolidateUntilDone asm
                         let ss'' = case substituteBoth casm (FunType ss') of
                                      FunType x -> x
                                      _ -> error "Substitution changed type in typeOfValue"
                         ss''' <- monomorphize (PolyFunctionType ids ss'')
                         return $ FunType ss'''
    where substituteBoth (Assumptions a b) = substituteUntilDone a . substituteStackUntilDone b

-- A single statement always carries an effect on the stack, hence we
-- treat its type as a function type.
typeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
          Statement -> StateT (Set Id) m PolyFunctionType
typeOf stmt = case stmt of
                Call v -> do
                          fn <- ask >>= lookupFn' v
                          case readerFunctionType fn of
                            Just x -> return x
                            Nothing -> throwError NotAFunction
                Literal d -> do
                          d' <- typeOfValue d
                          let quants = allQuantVars d'
                              r' = head [v | n <- [0 :: Int ..]
                                           , let v = Id "R" <> Id (show n)
                                           , not (v `elem` quants)]
                              fn = functionType [] (RestQuant r') [d'] (RestQuant r')
                          return $ PolyFunctionType (allQuantVars $ FunType fn) fn

typeOfSeq :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
             Sequence -> StateT (Set Id) m PolyFunctionType
typeOfSeq (Sequence xs) = foldM go emptyPolyFnType xs
    where go acc z = do
            knownVars $ quantifiedVars acc
            z' <- typeOf z
            knownVars $ quantifiedVars z'
            res <- composePFunctions acc z'
            knownVars $ quantifiedVars res
            return res

-- When checking the declared type of a thing, all we care about is
-- that the types line up. The assumptions we used to get there are
-- irrelevant.
checkDeclaredType :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                     PolyFunctionType -> Function -> m ()
checkDeclaredType (PolyFunctionType ids declared) (Function _ ss) = runWriterT go >>= doSub
    where go = do
            ss' <- evalStateT (typeOfSeq ss) mempty
            let inferred = underlyingFnType ss'
            inferred `isFnSubtypeOf` toGround' ids declared
          toGround' i fn = case toGround i (FunType fn) of
                             FunType fn' -> fn'
                             _ -> error "toGround changed shape of type in checkDeclaredType"
          doSub ((), w) = do
                 Assumptions _ _ <- consolidateUntilDone w
                 pure ()

checkTypeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m) => Id -> ReaderValue -> m ()
checkTypeOf _ (UDFunction t f) = checkDeclaredType t f
checkTypeOf _ (BIFunction _ _) = pure () -- We don't typecheck primitives.
checkTypeOf _ (Module m) = checkTypes m -- Nothing to do with the module as a whole (yet).

checkTypes :: (MonadError FactorError m, MonadReader ReadOnlyState m) => Map Id ReaderValue -> m ()
checkTypes = void . Map.traverseWithKey checkTypeOf

checkAllTypes :: (MonadError FactorError m, MonadReader ReadOnlyState m) => m ()
checkAllTypes = asks readerNames >>= checkTypes
