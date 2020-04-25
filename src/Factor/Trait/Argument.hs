{-# LANGUAGE FlexibleContexts #-}

module Factor.Trait.Argument(ModuleArg(..), TraitRef(..),
                             moduleArgName, moduleArgTraitName, traitRefName, traitRefArgs,
                             subArg, subArgInType, subArgInFnType, subArgInPolyFnType,
                             subArgInStmt, subArgInSeq, subArgInFunctorInfo,
                             substituteTrait) where

import Factor.Trait.Types
import Factor.Type
import Factor.Id
import Factor.Code

import Control.Lens

subArg :: (Id -> QId) -> QId -> QId
subArg _ (QId []) = QId [] -- WTF?
subArg f (QId (y:ys)) = f y <> QId ys

subArgInType :: (Id -> QId) -> Type -> Type
subArgInType f t =
    case t of
      FunType fn -> FunType (subArgInFnType f fn)
      ModuleType q -> ModuleType (subArg f q)
      GroundVar i -> GroundVar i
      QuantVar i -> QuantVar i

subArgInFnType :: (Id -> QId) -> FunctionType -> FunctionType
subArgInFnType f (FunctionType (StackDesc args a) (StackDesc rets r)) =
    FunctionType (StackDesc (fmap (subArgInType f) args) a)
                 (StackDesc (fmap (subArgInType f) rets) r)

subArgInPolyFnType :: (Id -> QId) -> PolyFunctionType -> PolyFunctionType
subArgInPolyFnType f (PolyFunctionType bindings t) =
    PolyFunctionType bindings $ subArgInFnType f t

subArgInData :: (Id -> QId) -> Data -> Data
subArgInData f lit = case lit of
                       Int {} -> lit
                       FunctionValue (Function v ss) -> FunctionValue $ Function v (subArgInSeq f ss)
                       Bool {} -> lit
                       String {} -> lit
                       Symbol {} -> lit
                       RecordInstance qid arr -> RecordInstance (subArg f qid) (fmap (subArgInData f) arr)

subArgInStmt :: (Id -> QId) -> Statement -> Statement
subArgInStmt f (Call qid) = Call (subArg f qid)
subArgInStmt f (Literal lit) = Literal (subArgInData f lit)

subArgInSeq :: (Id -> QId) -> Sequence -> Sequence
subArgInSeq f (Sequence xs) = Sequence $ fmap (subArgInStmt f) xs

subArgInFunctorInfo :: (Id -> QId) -> FunctorInfo -> FunctorInfo
subArgInFunctorInfo f (FunctorUDFunction p (Function t ss)) =
    FunctorUDFunction (subArgInPolyFnType f p) (Function t (subArgInSeq f ss))
subArgInFunctorInfo f (FunctorUDMacro p (Macro t ss)) =
    FunctorUDMacro (subArgInPolyFnType f p) (Macro t (subArgInSeq f ss))
subArgInFunctorInfo f (FunctorModule m) =
    FunctorModule $ fmap (subArgInFunctorInfo f) m
subArgInFunctorInfo f (FunctorTrait (ParameterizedTrait args t)) =
    let f' v = if any (\(ModuleArg v' _) -> v == v') args then QId [v] else f v
    in -- TODO Substitute in args
       FunctorTrait (ParameterizedTrait args (substituteTrait f' t))
subArgInFunctorInfo f (FunctorFunctor args t) =
    let f' v = if any (\(ModuleArg v' _) -> v == v') args then QId [v] else f v
    in -- TODO Substitute in args
       FunctorFunctor args (fmap (subArgInFunctorInfo f') t)
--subArgInFunctorInfo _ FunctorDemandType = FunctorDemandType

substituteTrait :: (Id -> QId) -> Trait -> Trait
substituteTrait f (Trait ts) = Trait $ fmap (_2 %~ go) ts
    where go info =
              case info of
                TraitFunction p -> TraitFunction (subArgInPolyFnType f p)
                TraitMacro p -> TraitMacro (subArgInPolyFnType f p)
                TraitModule xs -> TraitModule $ fmap (_2 %~ go) xs
                TraitInclude (TraitRef q args) -> TraitInclude (TraitRef q (fmap (subArg f) args))
--                TraitDemandType -> TraitDemandType
                TraitFunctor args xs ->
                    let handleArg (ModuleArg s (TraitRef q innerargs)) =
                            ModuleArg s (TraitRef q $ fmap (subArg f) innerargs)
                        argnames = fmap (\(ModuleArg s _) -> s) args
                        args' = fmap handleArg args
                        f' v = if v `elem` argnames then QId [v] else f v
                        Trait xs' = substituteTrait f' (Trait xs)
                    in TraitFunctor args' xs'
