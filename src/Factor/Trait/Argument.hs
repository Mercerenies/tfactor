{-# LANGUAGE FlexibleContexts #-}

module Factor.Trait.Argument(ModuleArg(..), TraitRef(..),
                             moduleArgName, moduleArgTraitName, traitRefName, traitRefArgs,
                             subArg, subArgInType, subArgInFnType, subArgInPolyFnType,
                             substituteTrait) where

import Factor.Trait.Types
import Factor.Type
import Factor.Id

import Control.Lens

subArg :: (Id -> QId) -> QId -> QId
subArg _ (QId []) = QId [] -- WTF?
subArg f (QId (y:ys)) = f y <> QId ys

subArgInType :: (Id -> QId) -> Type -> Type
subArgInType f t =
    case t of
      FunType fn -> FunType (subArgInFnType f fn)
      NamedType q -> NamedType (subArg f q)
      GroundVar i -> GroundVar i
      QuantVar i -> QuantVar i

subArgInFnType :: (Id -> QId) -> FunctionType -> FunctionType
subArgInFnType f (FunctionType (StackDesc args a) (StackDesc rets r)) =
    FunctionType (StackDesc (fmap (subArgInType f) args) a)
                 (StackDesc (fmap (subArgInType f) rets) r)

subArgInPolyFnType :: (Id -> QId) -> PolyFunctionType -> PolyFunctionType
subArgInPolyFnType f (PolyFunctionType bindings t) =
    PolyFunctionType bindings $ subArgInFnType f t

substituteTrait :: (Id -> QId) -> Trait -> Trait
substituteTrait f (Trait ts) = Trait $ fmap (_2 %~ go) ts
    where go info =
              case info of
                TraitFunction p -> TraitFunction (subArgInPolyFnType f p)
                TraitMacro p -> TraitMacro (subArgInPolyFnType f p)
                TraitModule xs -> TraitModule $ fmap (_2 %~ go) xs
                TraitInclude (TraitRef q args) -> TraitInclude (TraitRef q (fmap (subArg f) args))
                TraitDemandType -> TraitDemandType
