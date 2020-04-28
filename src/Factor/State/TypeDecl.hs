{-# LANGUAGE FlexibleContexts #-}

module Factor.State.TypeDecl(TypeToDeclare(..), declareType) where

import Factor.Id
import Factor.Trait.Types
import Factor.State.Types
import Factor.State.Resource
import qualified Factor.Stack as Stack
import Factor.Error
import Factor.Code
import Factor.Names
import Factor.Type
import Factor.Util

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Lens

data TypeToDeclare = TypeToDeclare {
      typeDeclContainer :: QId,
      typeDeclName :: Id,
      typeDeclArgs :: [Id],
      typeDeclInfo :: [TypeInfo]
    } deriving (Show, Eq)

declareType :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
               TypeToDeclare -> Module -> m Module
declareType (tdecl @ (TypeToDeclare qid v vs info)) reader = do
  let qid' = qid <> QId [v]
      names = [patternname] ++ fmap (\(TypeVal name _) -> name) info
      patternname = Id "*" <> v
  forM_ names $ \name ->
      when (Map.member name (reader^.moduleNames)) $ throwError (DuplicateDecl name)
  case containsDuplicate names of
    Just name -> throwError (DuplicateDecl name)
    _ -> pure ()
  let res = TypeValue (TypeData $ length vs)
  reader' <- traverseOf moduleNames (defineResource qid' v res) reader
  reader'' <- bindConstructors tdecl reader'
  bindPattern tdecl patternname reader''

bindConstructors :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
                    TypeToDeclare -> Module -> m Module
bindConstructors (TypeToDeclare qid v vs ts) reader0 = foldM go reader0 (zip [0 :: Int ..] ts)
    where desttype = NamedType (TypeId (qid <> QId [v]) (fmap QuantVar vs))
          go reader (idx, TypeVal n ss) =
              let usedvars = vs ++ (concatMap allQuantVars $ Stack.toList ss)
                  restvar = freshVar "R" usedvars
                  restvar' = RestQuant restvar
                  fntype = FunctionType (StackDesc ss restvar') (StackDesc (Stack.singleton desttype) restvar')
                  pfntype = PolyFunctionType (restvar : usedvars) fntype
                  impl = Sequence [
                          Literal (Int . toInteger $ Stack.length ss),
                          Literal (Int . toInteger $ idx),
                          Literal (String $ qidName (qid <> QId [v])),
                          Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-construct"]), -- TODO Put this name in Factor.Names
                          Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
                         ]
                  qidn = qid <> QId [n]
              in traverseOf moduleNames (defineResource qidn n (UDFunction pfntype $ Function (Just n) impl)) reader

bindPattern :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
               TypeToDeclare -> Id -> Module -> m Module
bindPattern (TypeToDeclare qid tname vs ts) pname reader =
      traverseOf moduleNames (defineResource qidn pname (UDFunction pfntype $ Function (Just pname) impl)) reader
    where typename = NamedType (TypeId (qid <> QId [tname]) (fmap QuantVar vs))
          qidn = qid <> QId [pname]
          usedvars = vs
          restvar1 = freshVar "S" usedvars
          restvar2 = freshVar "T" usedvars
          destructorFor (TypeVal _ ss) = FunctionType (StackDesc ss (RestQuant restvar1))
                                                      (StackDesc Stack.empty (RestQuant restvar2))
          allDestructors = fmap (FunType . destructorFor) ts
          args = StackDesc (Stack.fromList (reverse allDestructors) <> Stack.singleton typename) (RestQuant restvar1)
          rets = StackDesc Stack.empty (RestQuant restvar2)
          fntype = FunctionType args rets
          pfntype = PolyFunctionType ([restvar1, restvar2] ++ usedvars) fntype
          impl = Sequence [
                  Literal (Int . toInteger $ length ts),
                  Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-branch"]) -- TODO Factor.Names
                 ]
