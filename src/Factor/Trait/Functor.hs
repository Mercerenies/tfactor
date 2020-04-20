{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Trait.Functor(bindModule) where

import Factor.Trait
import Factor.Trait.Types
import Factor.Trait.Argument
import Factor.State.Reader
import Factor.State.Resource
import Factor.Error
import Factor.Id

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map

bindModule :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
              QId -> QId -> ParameterizedModule -> [QId] -> m RId
bindModule mqid fnqid (ParameterizedModule params info) args
    | length params /= length args = throwError $ FunctorArgError fnqid (length params) (length args)
    | otherwise = do
        zipped <- forM (zip params args) $ \(ModuleArg param (TraitRef req innerargs), arg) -> do
                    modl <- get >>= lookupFn arg >>= \case
                            ModuleValue m -> pure m
                            _ -> throwError (NoSuchModule arg)
                    req' <- get >>= lookupFn req >>= \case
                            TraitValue pt -> get >>= runReaderT (bindTrait req pt innerargs)
                            _ -> throwError (NoSuchTrait req)
                    get >>= runReaderT (moduleSatisfies' req' modl)
                    return (param, arg)
        let submap = Map.fromList zipped
            subfn k = maybe (QId [k]) id (Map.lookup k submap)
        modl <- Map.traverseWithKey (\k v -> bindFunctorInfo subfn (mqid <> QId [k]) v) info
        appendResourceRO' mqid (ModuleValue $ mapToModule modl)

bindFunctorInfo :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                   (Id -> QId) -> QId -> FunctorInfo -> m RId
bindFunctorInfo subfn qid info =
    case info of
      -- TODO Substitute in the function sequences too
      FunctorUDFunction ptype fn -> appendResourceRO' qid (UDFunction (subArgInPolyFnType subfn ptype) fn)
      FunctorUDMacro ptype fn -> appendResourceRO' qid (UDMacro (subArgInPolyFnType subfn ptype) fn)
      FunctorModule m -> do
                modl <- Map.traverseWithKey (\k v -> bindFunctorInfo subfn (qid <> QId [k]) v) m
                appendResourceRO' qid (ModuleValue $ mapToModule modl)
      FunctorTrait (ParameterizedTrait args t) ->
          let -- We don't want to substitute any names bound by the
              -- (parameterized) trait itself.
              subfn' k = if any (\(ModuleArg k' _) -> k == k') args then QId [k] else subfn k
          in -- TODO Substitute in args
             appendResourceRO' qid (TraitValue (ParameterizedTrait args (substituteTrait subfn' t)))

appendResourceRO' :: MonadState ReadOnlyState m => QId -> ReaderValue -> m RId
appendResourceRO' qid value = state (appendResourceRO qid value)
