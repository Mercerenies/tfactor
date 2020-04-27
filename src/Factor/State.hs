{-# LANGUAGE FlexibleContexts, ViewPatterns, KindSignatures, RankNTypes, TypeFamilies, ScopedTypeVariables #-}

module Factor.State(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    Module(Module), ModuleDecl(..),
                    readerModule, readerNames, readerResources,
                    moduleNames, moduleDecls,
                    newReader, emptyModule, mapToModule,
                    declsToReadOnly) where

import Factor.Error
import Factor.Code
import Factor.Code.Decl
import Factor.Id
import Factor.Type
import Factor.State.Types
import Factor.State.Resource
import Factor.Util
import qualified Factor.Stack as Stack
import Factor.Names

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Monad.State
import Control.Lens

declsToReadOnly :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
                   QId -> [Declaration] -> Module -> m Module
declsToReadOnly qid ds r = foldM go r ds
    where go reader decl =
              case decl of
                _
                 | Just v <- newDeclName decl
                 , Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def) ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineFunction qid' v t def) reader
                MacroDecl t (Macro v def) ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineMacro qid' v t def) reader
                ModuleDecl v def -> do
                     let qid' = qid <> QId [v]
                     inner <- declsToReadOnly qid' def emptyModule
                     traverseOf moduleNames (defineModule qid' v inner) reader
                TypeDecl v info -> do
                     let names = fmap (\(TypeVal name _) -> name) info
                     forM_ names $ \name ->
                         when (Map.member name (reader^.moduleNames)) $ throwError (DuplicateDecl name)
                     case containsDuplicate names of
                       Just name -> throwError (DuplicateDecl name)
                       _ -> pure ()
                     reader' <- traverseOf moduleNames (defineResource (qid <> QId [v]) v TypeValue) reader
                     reader'' <- bindConstructors qid v info reader'
                     return reader''
                ModuleSyn v dest -> pure $ over moduleDecls (++ [ModuleSynonym v dest]) reader
                -- RecordDecl v def
                --  | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                --  | otherwise -> do
                --           let qid' = qid <> QId [v]
                --           inner <- expandRecordDecl qid' def emptyModule
                --           let inner' = set moduleType (Just (TypeProperties TAny)) inner
                --           traverseOf moduleNames (defineModule qid' v inner') reader
                TraitDecl v def ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineResource qid' v (TraitValue def)) reader
                FunctorDecl v def ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineResource qid' v (FunctorValue def)) reader
                -- RecordFunctorDecl v args def
                --  | Map.member v (reader^.moduleNames) -> throwError (DuplicateDecl v)
                --  | otherwise ->
                --      let qid' = qid <> QId [v]
                --          functor = expandRecordFunDecl qid' def (Map.fromList [(Id "", FunctorDemandType)]) -- TODO 'Id ""'..... yeah
                --          rv = FunctorValue (ParameterizedModule args functor)
                --      in traverseOf moduleNames (defineResource qid' v rv) reader
                AliasDecl i j -> pure $ over moduleDecls (++ [Alias i j]) reader
                OpenDecl j -> pure $ over moduleDecls (++ [Open j]) reader
                RequireDecl j -> pure $ over moduleDecls (++ [AssertTrait j]) reader
                IncludeDecl q -> pure $ over moduleDecls (++ [IncludeModule q]) reader

bindConstructors :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
                    QId -> Id -> [TypeInfo] -> Module -> m Module
bindConstructors qid v ts reader0 = foldM go reader0 (zip [0 :: Int ..] ts)
    where desttype = ModuleType $ qid <> QId [v]
          go reader (idx, TypeVal n ss) =
              let usedvars = concatMap allQuantVars $ Stack.toList ss
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

newDeclName :: Declaration -> Maybe Id
newDeclName (FunctionDecl _ (Function Nothing _)) = Nothing
newDeclName (FunctionDecl _ (Function (Just v) _)) = Just v
newDeclName (MacroDecl _ (Macro v _)) = Just v
newDeclName (ModuleDecl v _) = Just v
newDeclName (ModuleSyn {}) = Nothing -- Doesn't define a resource yet; it'll get expanded later.
newDeclName (TraitDecl v _) = Just v
newDeclName (FunctorDecl v _) = Just v
newDeclName (TypeDecl v _) = Just v
newDeclName (AliasDecl {}) = Nothing
newDeclName (OpenDecl {}) = Nothing
newDeclName (RequireDecl {}) = Nothing
newDeclName (IncludeDecl {}) = Nothing

defineFunction :: MonadState (ResourceTable ReaderValue) m =>
                  QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineFunction q v t def = defineResource q v (UDFunction t $ Function (Just v) def)

defineMacro :: MonadState (ResourceTable ReaderValue) m =>
               QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineMacro q v t def = defineResource q v (UDMacro t $ Macro v def)

defineModule :: MonadState (ResourceTable ReaderValue) m =>
                QId -> Id -> Module -> Map Id RId -> m (Map Id RId)
defineModule q v def = defineResource q v (ModuleValue def)

-- TODO We have to have two record expansions, one for functors and
-- one for modules. I get that. But surely *some* parts of them can be
-- consolidated and made into helper functions, so they're not both
-- just behemoths of 80% identical code.

-- Hopefully, somewhere down the road, record declarations will be
-- able to be a declaration macro defined in Prelude, and then they'll
-- just translate inside the language. But for now, it's special
-- syntax sugar since the tools to do that aren't in place yet.
-- expandRecordDecl :: (MonadState (ResourceTable ReaderValue) m, MonadError FactorError m) =>
--                     QId -> [RecordInfo] -> Module -> m Module
-- expandRecordDecl qid ds r = foldM go r ds
--     where collectField (RecordField i t) = Just (i, t)
--           collectField _ = Nothing
--           collectedFields = mapMaybe collectField ds
--           usedTypeVars = concatMap (\(_, t) -> allQuantVars t) collectedFields
--           unusedTypeVar = freshVar "R" usedTypeVars
--           go reader decl =
--               case decl of
--                 RecordConstructor v ->
--                     let var = unusedTypeVar
--                         args = reverse $ fmap (\(_, t) -> t) collectedFields
--                         fntype = polyFunctionType [var] args (RestQuant var)
--                                                         [ModuleType qid] (RestQuant var)
--                         impl = Sequence [
--                                 Literal (Int . toInteger $ length collectedFields),
--                                 Literal (String $ qidName qid),
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-construct"]), -- TODO Put this name in Factor.Names
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
--                                ]
--                         d = FunctionDecl fntype (Function (Just v) impl)
--                     in declsToReadOnly qid [d] reader
--                 RecordField i t ->
--                     let var = unusedTypeVar
--                         fntype = polyFunctionType [var] [ModuleType qid] (RestQuant var)
--                                                         [t] (RestQuant var)
--                         -- We're iterating over the exact same data
--                         -- used to construct collectedFields, so the
--                         -- field must exist.
--                         indx = maybe (error "Internal error in expandRecordDecl") id
--                                 (List.findIndex (\(i', _) -> i == i') collectedFields)
--                         impl = Sequence [
--                                 Literal (Int . toInteger $ indx),
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-get"]), -- TODO Put this name in Factor.Names
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
--                                ]
--                         d = FunctionDecl fntype (Function (Just i) impl)
--                     in declsToReadOnly qid [d] reader
--                 RecordOrdinaryDecl d -> declsToReadOnly qid [d] reader

-- expandRecordFunDecl :: QId -> [RecordFunInfo] -> Map Id FunctorInfo -> Map Id FunctorInfo
-- expandRecordFunDecl qid ds r = foldl go r ds
--     where collectField (RecordFunField i t) = Just (i, t)
--           collectField _ = Nothing
--           collectedFields = mapMaybe collectField ds
--           usedTypeVars = concatMap (\(_, t) -> allQuantVars t) collectedFields
--           unusedTypeVar = freshVar "R" usedTypeVars
--           go reader decl =
--               case decl of
--                 RecordFunConstructor v ->
--                     let var = unusedTypeVar
--                         args = reverse $ fmap (\(_, t) -> t) collectedFields
--                         fntype = polyFunctionType [var] args (RestQuant var)
--                                                         -- TODO Put Self in Factor.Names
--                                                         [ModuleType (QId [Id "Self"])] (RestQuant var)
--                         impl = Sequence [
--                                 Literal (Int . toInteger $ length collectedFields),
--                                 Literal (String $ qidName qid), -- TODO This is the wrong name!
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-construct"]), -- TODO Put this name in Factor.Names
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
--                                ]
--                         d = FunctorUDFunction fntype (Function (Just v) impl)
--                     in Map.insert v d reader
--                 RecordFunField i t ->
--                     let var = unusedTypeVar
--                         fntype = polyFunctionType [var] [ModuleType (QId [Id "Self"])] (RestQuant var)
--                                                         [t] (RestQuant var)
--                         -- We're iterating over the exact same data
--                         -- used to construct collectedFields, so the
--                         -- field must exist.
--                         indx = maybe (error "Internal error in expandRecordDecl") id
--                                 (List.findIndex (\(i', _) -> i == i') collectedFields)
--                         impl = Sequence [
--                                 Literal (Int . toInteger $ indx),
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe-record-get"]), -- TODO Put this name in Factor.Names
--                                 Call (QId [rootAliasName, primitivesModuleName, Id "unsafe1"]) -- TODO Put this name in Factor.Names
--                                ]
--                         d = FunctorUDFunction fntype (Function (Just i) impl)
--                     in Map.insert i d reader
--                 RecordFunOrdinaryDecl i d -> Map.insert i d reader
