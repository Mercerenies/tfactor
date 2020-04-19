{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, RankNTypes #-}

module Factor.State.Reader(ReadOnlyState(ReadOnlyState), ReaderValue(..),
                           Module(Module), ModuleDecl(..),
                           readerModule, readerNames, readerResources,
                           moduleNames, moduleDecls, moduleIsType,
                           newReader, emptyModule, mapToModule,
                           atQIdResource, atQId, lookupFn, lookupFnName,
                           allNamesInModule, allNames,
                           readerFunctionType, readerMacroType,
                           merge, mapToReader) where

import Factor.State.Types
import Factor.State.Resource
import Factor.Error
import Factor.Id
import Factor.Type

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Foldable
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge

-- TODO Is this whole thing valid from a Traversal law standpoint...?

atRId :: RId -> Traversal' ReadOnlyState ReaderValue
atRId rid = readerResources . ix rid

moduleHelper :: Traversal' ReaderValue Module
moduleHelper f (ModuleValue m) = ModuleValue <$> f m
moduleHelper _ x = pure x

atQIdResource0 :: forall f. Applicative f => QId -> (RId -> f RId) -> ReadOnlyState -> f ReadOnlyState
atQIdResource0 (QId xs0) f r0 = go xs0 readerNames
    where go :: [Id] -> Traversal' ReadOnlyState (Map Id RId) -> f ReadOnlyState
          go [] _ = pure r0
          go [x] r = (r . ix x) f r0
          go (x:xs) r = case Map.lookup x (r0^.r) of
                          Nothing -> pure r0
                          Just rid -> go xs (readerResources . ix rid . moduleHelper . moduleNames)

-- So for whatever reason, Haskell needed some help (in the form of
-- ScopedTypeVariables) getting the type signatures of atQIdResource0
-- right. So the type signature up there is messy, and this function
-- acts simply as an assertion that it's still the traversal that I
-- want.
atQIdResource :: QId -> Traversal' ReadOnlyState RId
atQIdResource = atQIdResource0

atQId :: QId -> Traversal' ReadOnlyState ReaderValue
atQId (QId xs0) f r0 = go xs0 (r0^.readerNames)
    where go [] _ = pure r0
          go [x] r = case Map.lookup x r of
                       Nothing -> pure r0
                       Just rid -> atRId rid f r0
          go (x:xs) r = case Map.lookup x r >>= flip getResource (r0^.readerResources) of
                          Nothing -> pure r0
                          Just (ModuleValue m) -> go xs (m^.moduleNames)
                          Just _ -> pure r0

-- TODO This is used for more than just functions. Change its name to
-- reflect that, and make it stop throwing NoSuchFunction, since that
-- error may not be right.
lookupFn :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn (QId ids) reader =
  let go (ModuleValue names) i =
          case (names^.moduleNames.at i) >>= flip getResource (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> pure x
      go _ _ = throwError $ NoSuchModule (QId ids)
  in foldM go (ModuleValue $ view readerModule reader) ids

lookupFnName :: MonadError FactorError m => QId -> ReadOnlyState -> m QId
lookupFnName (QId ids) reader =
  let go (ModuleValue names) [i] =
          case (names^.moduleNames.at i) >>= flip getResourceName (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> pure x
      go (ModuleValue names) (i:is) =
          case (names^.moduleNames.at i) >>= flip getResource (reader^.readerResources) of
            Nothing -> throwError $ NoSuchFunction (QId ids)
            Just x -> go x is
      go _ _ = throwError $ NoSuchModule (QId ids)
  in go (ModuleValue $ view readerModule reader) ids


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
              in k : innernames
          go' :: Id -> RId -> [QId]
          go' k v = case getResource v resources of
                      Nothing -> []
                      Just v' -> go k v'

allNames :: ReadOnlyState -> [QId]
allNames reader = allNamesInModule (reader^.readerResources) (QId []) (reader^.readerModule)

-- TODO Internally combine these into one function, since they're
-- mutually exclusive, then provide the same public API in a more
-- consolidated way.

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType (UDFunction t _) = Just t
readerFunctionType (BIFunction t _) = Just t
readerFunctionType (UDMacro _ _) = Nothing
readerFunctionType (ModuleValue _) = Nothing
readerFunctionType (TraitValue _) = Nothing

readerMacroType :: ReaderValue -> Maybe PolyFunctionType
readerMacroType (UDFunction _ _) = Nothing
readerMacroType (BIFunction _ _) = Nothing
readerMacroType (UDMacro t _) = Just t
readerMacroType (ModuleValue _) = Nothing
readerMacroType (TraitValue _) = Nothing

merge :: MonadError FactorError m => ReadOnlyState -> ReadOnlyState -> m ReadOnlyState
merge (ReadOnlyState (Module m a t) r) (ReadOnlyState (Module m' a' t') r') =
    ReadOnlyState <$> (Module <$> merged <*> pure (a ++ a') <*> pure (t || t')) <*> pure rtable
        where failure = Merge.zipWithAMatched $ \k _ _ -> throwError (DuplicateDecl k)
              renamedm = fmap (+ resourceCount r) m'
              renamedr = modifyRIds (+ resourceCount r) r'
              merged = Merge.mergeA Merge.preserveMissing Merge.preserveMissing failure m renamedm
              rtable = r `catResources` renamedr

mapToReader :: Map Id ReaderValue -> ReadOnlyState
mapToReader m = ReadOnlyState (mapToModule modl) resources
    where (modl, resources) = runState (Map.traverseWithKey go m) newResourceTable
          go k v = appendResource' (QId [k]) v