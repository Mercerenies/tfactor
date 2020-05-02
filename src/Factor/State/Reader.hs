{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RankNTypes #-}

module Factor.State.Reader(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                           Module(Module), SynonymType(..), ModuleDecl(..), TypeData(..),
                           readerModule, readerNames, readerResources,
                           moduleNames, moduleDecls,
                           newReader, emptyModule, mapToModule,
                           atQIdResource, atQId, lookupFn, lookupFnName,
                           allNamesInModule, allNames, allChildrenOf,
                           readerFunctionType, readerMacroType,
                           merge, mapToReader) where

import Factor.State.Types
import Factor.State.Resource
import Factor.Error
import Factor.Id
import Factor.Type
import Factor.Names
import Factor.Util

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Foldable
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import Data.Maybe(listToMaybe)

-- TODO Is this whole thing valid from a Traversal law standpoint...?

--atRId :: RId -> Traversal' ReadOnlyState ReaderValue
--atRId rid = readerResources . ix rid

moduleHelper :: Traversal' ReaderValue Module
moduleHelper f (ModuleValue m) = ModuleValue <$> f m
moduleHelper _ x = pure x

atQIdResourceGeneral :: forall f. Applicative f => QId ->
                        ((RId, QId, ReaderValue) -> f (RId, QId, ReaderValue)) ->
                        ReadOnlyState -> f ReadOnlyState
atQIdResourceGeneral (QId xs0) f r0 = go xs0' readerNames
    where xs0' = if listToMaybe xs0 == Just rootAliasName then tail xs0 else xs0
          go :: [Id] -> Traversal' ReadOnlyState (Map Id RId) -> f ReadOnlyState
          go [] _ = pure r0
          go [x] r =
              case Map.lookup x (r0^.r) >>= \rid -> (,) rid <$> getResourceFull rid (r0^.readerResources) of
                Nothing -> pure r0
                Just (rid, (q, rv)) -> (\(rid', q', rv') -> over readerResources (modifyResourceFull (const (q', rv')) rid) $ over r (Map.insert x rid') r0) <$> f (rid, q, rv)
          go (x:xs) r = case Map.lookup x (r0^.r) of
                          Nothing -> pure r0
                          Just rid -> go xs (readerResources . ix rid . moduleHelper . moduleNames)

atQIdResource :: QId -> Traversal' ReadOnlyState RId
atQIdResource q = atQIdResourceGeneral q . _1

atQId :: QId -> Traversal' ReadOnlyState ReaderValue
atQId q = atQIdResourceGeneral q . _3

-- TODO This is used for more than just functions. Change its name to
-- reflect that, and make it stop throwing NoSuchFunction, since that
-- error may not be right.
lookupFn :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn q reader = case reader^.possibly (atQIdResourceGeneral q._3) of
                      Nothing -> throwError $ NoSuchFunction q
                      Just x -> pure x

lookupFnName :: MonadError FactorError m => QId -> ReadOnlyState -> m QId
lookupFnName q reader = case reader^.possibly (atQIdResourceGeneral q._2) of
                          Nothing -> throwError $ NoSuchFunction q
                          Just x -> pure x


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
                                 TraitValue {} -> []
                                 FunctorValue {} -> [] -- Nothing allocated yet.
                                 TypeValue {} -> []
                                 SynonymPlaceholder {} -> [] -- Nothing visible yet.
              in k : innernames
          go' :: Id -> RId -> [QId]
          go' k v = case getResource v resources of
                      Nothing -> []
                      Just v' -> go k v'

allNames :: ReadOnlyState -> [QId]
allNames reader = allNamesInModule (reader^.readerResources) (QId []) (reader^.readerModule)

allChildrenOf :: ReadOnlyState -> QId -> [QId]
allChildrenOf reader qid =
    case lookupFn qid reader of
      Left _ -> [qid]
      Right (ModuleValue m) -> qid : concatMap go (Map.toList $ m^.moduleNames)
      Right _ -> [qid]
    where go (k, _) = allChildrenOf reader (qid <> QId [k])

data ReaderType = NoType | RFunType PolyFunctionType | RMacroType PolyFunctionType

readerType :: ReaderValue -> ReaderType
readerType (UDFunction t _) = RFunType t
readerType (BIFunction t _) = RFunType t
readerType (UDMacro t _) = RMacroType t
readerType (ModuleValue _) = NoType
readerType (TraitValue _) = NoType
readerType (FunctorValue _) = NoType
readerType (TypeValue _) = NoType
readerType (SynonymPlaceholder _) = NoType

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType v = case readerType v of
                         RFunType t -> Just t
                         _ -> Nothing

readerMacroType :: ReaderValue -> Maybe PolyFunctionType
readerMacroType v = case readerType v of
                      RMacroType t -> Just t
                      _ -> Nothing

merge :: MonadError FactorError m => ReadOnlyState -> ReadOnlyState -> m ReadOnlyState
merge (ReadOnlyState (Module m a) r) (ReadOnlyState (Module m' a') r') =
    ReadOnlyState <$> (Module <$> merged <*> pure (a ++ a')) <*> pure rtable
        where failure = Merge.zipWithAMatched $ \k _ _ -> throwError (DuplicateDecl k)
              renamedm = fmap (+ resourceCount r) m'
              renamedr = modifyRIds (+ resourceCount r) r'
              merged = Merge.mergeA Merge.preserveMissing Merge.preserveMissing failure m renamedm
              rtable = r `catResources` renamedr

mapToReader :: Map Id ReaderValue -> ReadOnlyState
mapToReader m = ReadOnlyState (mapToModule modl) resources
    where (modl, resources) = runState (Map.traverseWithKey go m) newResourceTable
          go k v = appendResource' (QId [k]) v
