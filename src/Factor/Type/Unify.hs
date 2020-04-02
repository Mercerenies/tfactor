{-# LANGUAGE FlexibleContexts #-}

module Factor.Type.Unify where

import Factor.Type
import Factor.Type.Error

import Control.Monad.Except
import Data.Function

-- This is a reminder to complete unification for any new types we
-- define. If it has a case defined, then we've written isSubtype for
-- that case.
_subtypeUnify :: Type -> ()
_subtypeUnify (PrimType {}) = ()
_subtypeUnify (FunType {}) = ()

isFnSubtypeOf :: (FromTypeError e, MonadError e m) => FunctionType -> FunctionType -> m ()
isFnSubtypeOf = isSubtypeOf `on` FunType

isSubtypeOf :: (FromTypeError e, MonadError e m) => Type -> Type -> m ()
isSubtypeOf t t' = go t t'
    where noUnify = throwError (fromTypeError $ CouldNotUnify t t')
          go (PrimType TNothing) _ = pure ()
          go _ (PrimType TAny) = pure ()
          go (PrimType p) (PrimType p') | p == p' = pure ()
          go (FunType (FunctionType args rets)) (FunType (FunctionType args' rets'))
              | length rets - length args /= length rets' - length args' = noUnify
              | length rets > length rets' = noUnify
              | otherwise = let n = length rets'
                                missing = n - length rets
                                -- Not a typo; we take from args'
                                -- twice. The idea is that a function
                                -- which ignores its Nth argument is
                                -- really just passing the argument to
                                -- the result, hence the argument type
                                -- equals the result type.
                                argspad = take missing args' ++ args
                                retspad = take missing args' ++ rets
                            in zipWithM_ isSubtypeOf retspad rets' >>
                               zipWithM_ isSubtypeOf args' argspad
          go _ _ = noUnify -- If nothing else applies, then it failed.

-- Left-to-right composition, so first function happens first.
composeFunctions :: (FromTypeError e, MonadError e m) =>
                    FunctionType -> FunctionType -> m FunctionType
composeFunctions f g = go (pad f g)
    where pad (FunctionType a b) (FunctionType b' c) =
              case length b `compare` length b' of
                GT ->
                    let missing = length b - length b'
                        extra = take missing b
                    in (FunctionType a b, FunctionType (extra ++ b') (extra ++ c))
                LT ->
                    let missing = length b' - length b
                        extra = take missing b'
                    in (FunctionType (extra ++ a) (extra ++ b), FunctionType b' c)
                EQ ->
                    -- They're already the correct dimensions, so pass through
                    (FunctionType a b, FunctionType b' c)
          go (FunctionType a b, FunctionType b' c)
              -- Precondition: We can assume by this point that
              -- (length b == length b').
              | length b /= length b' = error "Length precondition violated in composeFunctions"
              | otherwise =
                  zipWithM_ isSubtypeOf b b' >> pure (FunctionType a c)
