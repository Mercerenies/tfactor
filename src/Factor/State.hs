{-# LANGUAGE FlexibleContexts, ViewPatterns, KindSignatures, RankNTypes, TypeFamilies, ScopedTypeVariables #-}

module Factor.State(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    Module(Module), SynonymType(..), ModuleDecl(..),
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
import Factor.State.TypeDecl
import Factor.Trait.Types

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
