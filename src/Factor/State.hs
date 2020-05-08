{-# LANGUAGE FlexibleContexts, ViewPatterns, KindSignatures, RankNTypes,
  TypeFamilies, ScopedTypeVariables, LambdaCase #-}

module Factor.State(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    Module(Module), SynonymType(..), ModuleDecl(..),
                    readerModule, readerNames, readerResources,
                    moduleNames, moduleDecls,
                    newReader, emptyModule, mapToModule,
                    evalDecl, evalDecls,
                    declsToReadOnly) where

import Factor.Error
import Factor.Code
import Factor.Code.Decl
import Factor.Id
import Factor.Type
import Factor.State.Types
import Factor.State.Resource
import Factor.State.TypeDecl
import Factor.State.Reader
import Factor.Trait
import Factor.Trait.Types
import Factor.Util

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Monad.State
import Control.Lens

-- //// Pass an alias table through this and do alias resolution
evalDecl :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
            QId -> Declaration -> m ()
evalDecl qid decl = do
  get >>= checkDupName qid decl
  case decl of
    FunctionDecl _ (Function Nothing _) ->
        throwError (InternalError "Unnamed top-level function")
    FunctionDecl t (Function (Just v) def) -> do
        let qid' = qid <> QId [v]
        rid <- state (appendResourceRO qid' (UDFunction t $ Function (Just v) def))
        modifyM (defineAt qid' rid)
    MacroDecl t (Macro v def) -> do
        let qid' = qid <> QId [v]
        rid <- state (appendResourceRO qid' (UDMacro t $ Macro v def))
        modifyM (defineAt qid' rid)
    ModuleDecl v def -> do
        let qid' = qid <> QId [v]
        rid <- state (appendResourceRO qid' (ModuleValue emptyModule))
        modifyM (defineAt qid' rid)
        evalDecls qid' def
    TypeDecl v vs info ->
        modifyM . traverseOf (atCurrentModule qid) $ declareType' (TypeToDeclare qid v vs info)
    ModuleSyn v (Left name) ->
        let qid' = qid <> QId [v] in
        use (possibly $ atQIdResource name) >>= \case
        Nothing -> throwError (NoSuchModule name)
        Just rid -> modifyM (defineAt qid' rid)
    ModuleSyn v (Right (TraitRef dest args)) ->
        let qid' = qid <> QId [v] in
        get >>= lookupFn dest >>= \case
        FunctorValue pm -> do
                      rid <- bindModule qid' dest pm args
                      modifyM (defineAt qid' rid)
        _ -> throwError (NoSuchModule dest)
    RecordDecl i vs info -> evalDecl qid (desugarRecord i vs info)
    TraitDecl v def -> do
        let qid' = qid <> QId [v]
        rid <- state (appendResourceRO qid' (TraitValue def))
        modifyM (defineAt qid' rid)
    FunctorDecl v args decls -> do
        let qid' = qid <> QId [v]
            decls' = concatMapInMap modDeclToFunctorInfo decls
            pm = ParameterizedModule args decls'
        rid <- state (appendResourceRO qid' (FunctorValue pm))
        modifyM (defineAt qid' rid)
    AliasDecl _ _ -> pure () -- //// This
    OpenDecl _ -> pure () -- //// This
    RequireDecl j -> modifying (atCurrentModule qid.moduleDecls) (++ [AssertTrait j])
    IncludeDecl target -> do
        let go v rid = do
               let qid' = qid <> QId [v]
               reader <- get
               case lookupFn qid' reader of
                 Left _ -> pure ()
                 Right _ -> throwError (DuplicateDecl v)
               modifyM (defineAt qid' rid)
        modl <- get >>= lookupFn target >>= \case
                ModuleValue m -> pure m
                _ -> throwError (NoSuchModule target)
        void $ Map.traverseWithKey go (modl^.moduleNames)

evalDecls :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
             QId -> [Declaration] -> m ()
evalDecls qid decls = mapM_ (evalDecl qid) decls

atCurrentModule :: QId -> Traversal' ReadOnlyState Module
atCurrentModule qid = atQId qid . go
    where go f (ModuleValue m) = ModuleValue <$> f m
          go _ rv = pure rv

declareType' :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                TypeToDeclare -> Module -> m Module
declareType' t m = overLens readerResources (declareType t m)

checkDupName :: MonadError FactorError m =>
                QId -> Declaration -> ReadOnlyState -> m ()
checkDupName qid d r
    | Just v <- newDeclName d
    , Right _ <- lookupFn (qid <> QId [v]) r = throwError (DuplicateDecl v)
checkDupName _ _ _ = pure ()

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
                TypeDecl v vs info -> declareType (TypeToDeclare qid v vs info) reader
                ModuleSyn v dest ->
                     let qid' = qid <> QId [v]
                         syn = case dest of
                                 Left name -> SynonymGeneral name
                                 Right tr -> ActualizeFunctor tr
                     in traverseOf moduleNames (defineResource qid' v (SynonymPlaceholder syn)) reader
                RecordDecl i vs info -> go reader (desugarRecord i vs info)
                TraitDecl v def ->
                     let qid' = qid <> QId [v]
                     in traverseOf moduleNames (defineResource qid' v (TraitValue def)) reader
                FunctorDecl v args decls ->
                     let qid' = qid <> QId [v]
                         decls' = concatMapInMap modDeclToFunctorInfo decls
                         pm = ParameterizedModule args decls'
                     in traverseOf moduleNames (defineResource qid' v (FunctorValue pm)) reader
                AliasDecl i j -> pure $ over moduleDecls (++ [Alias i j]) reader
                OpenDecl j -> pure $ over moduleDecls (++ [Open j]) reader
                RequireDecl j -> pure $ over moduleDecls (++ [AssertTrait j]) reader
                IncludeDecl q -> pure $ over moduleDecls (++ [IncludeModule q]) reader

newDeclName :: Declaration -> Maybe Id
newDeclName (FunctionDecl _ (Function Nothing _)) = Nothing
newDeclName (FunctionDecl _ (Function (Just v) _)) = Just v
newDeclName (MacroDecl _ (Macro v _)) = Just v
newDeclName (ModuleDecl v _) = Just v
newDeclName (ModuleSyn v _) = Just v
newDeclName (RecordDecl v _ _) = Just v
newDeclName (TraitDecl v _) = Just v
newDeclName (FunctorDecl v _ _) = Just v
newDeclName (TypeDecl v _ _) = Just v
newDeclName (AliasDecl {}) = Nothing
newDeclName (OpenDecl {}) = Nothing
newDeclName (RequireDecl {}) = Nothing
newDeclName (IncludeDecl {}) = Nothing

concatMapInMap :: Ord k => (k -> a -> [(k, b)]) -> Map k a -> Map k b
concatMapInMap f = Map.fromList . concatMap (uncurry f) . Map.toList

modDeclToFunctorInfo :: Id -> ParamModuleDecl -> [(Id, FunctorInfo)]
modDeclToFunctorInfo i (PModFunction t f) = [(i, FunctorUDFunction t f)]
modDeclToFunctorInfo i (PModMacro t f) = [(i, FunctorUDMacro t f)]
modDeclToFunctorInfo i (PModModule m) = [(i, FunctorModule $ concatMapInMap modDeclToFunctorInfo m)]
modDeclToFunctorInfo i (PModFunctor args m) = [(i, FunctorFunctor args $ concatMapInMap modDeclToFunctorInfo m)]
modDeclToFunctorInfo i (PModTrait t) = [(i, FunctorTrait t)]
modDeclToFunctorInfo i (PModType v ts) = [(i, FunctorType v ts), (Id "*" <> i, FunctorGenerated)] -- TODO Factor.Names?
modDeclToFunctorInfo i (PModRecord vs info) = uncurry modDeclToFunctorInfo $ desugarRecordInFunctor i vs info

defineFunction :: MonadState (ResourceTable ReaderValue) m =>
                  QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineFunction q v t def = defineResource q v (UDFunction t $ Function (Just v) def)

defineMacro :: MonadState (ResourceTable ReaderValue) m =>
               QId -> Id -> PolyFunctionType -> Sequence -> Map Id RId -> m (Map Id RId)
defineMacro q v t def = defineResource q v (UDMacro t $ Macro v def)

defineModule :: MonadState (ResourceTable ReaderValue) m =>
                QId -> Id -> Module -> Map Id RId -> m (Map Id RId)
defineModule q v def = defineResource q v (ModuleValue def)
