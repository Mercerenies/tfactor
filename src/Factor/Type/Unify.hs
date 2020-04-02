{-# LANGUAGE FlexibleContexts #-}

module Factor.Type.Unify where

import Factor.Type
import Factor.Type.Error
import qualified Factor.Stack as Stack

import Control.Monad.Except
import Data.Function
import Data.Maybe(fromJust)

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
              | Stack.length rets - Stack.length args /= Stack.length rets' - Stack.length args' = noUnify
              | Stack.length rets > Stack.length rets' = noUnify
              | otherwise = let missing = Stack.length args' - Stack.length args
                                -- Not a typo; we take from args'
                                -- twice. The idea is that a function
                                -- which ignores its Nth argument is
                                -- really just passing the argument to
                                -- the result, hence the argument type
                                -- equals the result type. Also,
                                -- fromJust is safe here because we
                                -- already checked that the lengths
                                -- are correct in the guards.
                                extra = fromJust $ Stack.takeBottom missing args'
                                argspad = args <> extra
                                retspad = rets <> extra
                            in zipWithM_ isSubtypeOf (Stack.toList retspad) (Stack.toList rets') >>
                               zipWithM_ isSubtypeOf (Stack.toList args') (Stack.toList argspad)
          go _ _ = noUnify -- If nothing else applies, then it failed.

-- Left-to-right composition, so first function happens first.
composeFunctions :: (FromTypeError e, MonadError e m) =>
                    FunctionType -> FunctionType -> m FunctionType
composeFunctions f g = go (pad f g)
    where pad (FunctionType a b) (FunctionType b' c) =
              case Stack.length b `compare` Stack.length b' of
                GT ->
                    let missing = Stack.length b - Stack.length b'
                        extra = fromJust $ Stack.takeBottom missing b
                    in (FunctionType a b, FunctionType (b' <> extra) (c <> extra))
                LT ->
                    let missing = Stack.length b' - Stack.length b
                        extra = fromJust $ Stack.takeBottom missing b'
                    in (FunctionType (a <> extra) (b <> extra), FunctionType b' c)
                EQ ->
                    -- They're already the correct dimensions, so pass through
                    (FunctionType a b, FunctionType b' c)
          go (FunctionType a b, FunctionType b' c)
              -- Precondition: We can assume by this point that
              -- (length b == length b').
              | Stack.length b /= Stack.length b' = error "Length precondition violated in composeFunctions"
              | otherwise =
                  zipWithM_ isSubtypeOf (Stack.toList b) (Stack.toList b') >>
                  pure (FunctionType a c)
