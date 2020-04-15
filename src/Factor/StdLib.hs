{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Factor.StdLib(Prelude(),
                     builtins, stdlibs,
                     bindPrimitives, -- TODO This one will be private soon. I need it right now but I won't soon.
                     loadPrelude, bindStdlibModule) where

import Factor.Stack(Stack(..))
import qualified Factor.Stack as Stack
import Factor.State
import Factor.State.Alias
import Factor.State.Stack
import Factor.State.Types(BuiltIn(..))
import Factor.Id
import Factor.Type
import Factor.Error
import Factor.Code
import Factor.Eval
import Factor.Loader
import Factor.Parser.Token
import Factor.Parser
import Factor.Names

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable

newtype Prelude = Prelude ReadOnlyState

-- I've written it in several places, but I'm writing it here again to
-- be sure. `functionType', like `Stack.fromList' and the other
-- similar functions, treats the first element of a list as the top,
-- which is backwards from the way they're displayed. Thus, the stack
-- types here will look backward to how they would look if written in
-- the language.

popStack1 :: (MonadState EvalState m, MonadError FactorError m) => m Data
popStack1 = popStack 1 >>= \case
            Stack [x] -> pure x
            _ -> error "Internal error in popStack1"

popStack2 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data)
popStack2 = popStack 2 >>= \case
            Stack [x, y] -> pure (x, y)
            _ -> error "Internal error in popStack2"

popStack3 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data)
popStack3 = popStack 3 >>= \case
            Stack [x, y, z] -> pure (x, y, z)
            _ -> error "Internal error in popStack3"

_popStack4 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data)
_popStack4 = popStack 4 >>= \case
             Stack [x, y, z, t] -> pure (x, y, z, t)
             _ -> error "Internal error in popStack4"

_popStack5 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data, Data)
_popStack5 = popStack 5 >>= \case
            Stack [x, y, z, t, u] -> pure (x, y, z, t, u)
            _ -> error "Internal error in popStack5"

-- ( 'R Any -- 'R )
drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- ( 'R 'a -- 'R 'a 'a )
dup :: BuiltIn ()
dup = BuiltIn $ popStack1 >>= \x -> pushStack (Stack.fromList [x, x])

-- ( 'R 'a 'b -- 'R 'a 'b 'a )
over_ :: BuiltIn ()
over_ = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [y, x, y])

-- ( 'R 'a 'b 'c -- 'R 'a 'b 'c 'a )
pick :: BuiltIn ()
pick = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [z, x, y, z])

-- ( 'R 'a 'b -- 'R 'b 'a )
swap :: BuiltIn ()
swap = BuiltIn $ popStack 2 >>= (pushStack . Stack.reverse)

-- ( 'R 'a 'b 'c -- 'R 'b 'c 'a )
rot :: BuiltIn ()
rot = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [z, x, y])

-- ( 'S 'a ( 'S -- 'T ) -- 'T 'a )
dip :: BuiltIn ()
dip = BuiltIn $ popStack2 >>= \(f, x) ->
                assertFunction f >>= \(Function _ ss) ->
                evalSeq ss >> pushStack (Stack.fromList [x])

-- ( 'S ( 'S -- 'T ) -- 'T )
call :: BuiltIn ()
call = BuiltIn $ popStack1 >>= \case
       FunctionValue (Function _ ss) -> evalSeq ss
       _ -> throwError NotAFunction

-- ( 'S Bool ( 'S -- 'T ) ( 'S -- 'T ) -- 'T )
if_ :: BuiltIn ()
if_ = BuiltIn $ popStack3 >>= \(f, t, cond) -> do
        cond' <- assertBool cond
        Function _ t' <- assertFunction t
        Function _ f' <- assertFunction f
        evalSeq (if cond' then t' else f')

-- The function `unsafe` has no runtime effect but has a completely,
-- absurdly unsafe type signature. It will agree with literally any
-- function type. Needless to say, be VERY careful if you decide to
-- use it, as it's a backdoor into the type system explicitly designed
-- to break everything.

-- ( 'S -- 'T )
unsafe :: BuiltIn ()
unsafe = BuiltIn $ pure ()

-- ( 'S 'a -- 'S 'b )
unsafe1 :: BuiltIn ()
unsafe1 = BuiltIn $ pure ()

-- This function is also unsafe, in that its type signature is a lie.
-- To use it safely, you MUST wrap it in a function which explicitly
-- declares the CORRECT stack effect. It will actually pop as many
-- additional arguments as the integer tells it to. There's no way to
-- express this in the type system, so here we are.

-- ( 'S Int String -- 'T Any )
unsafeRecordConstruct :: BuiltIn ()
unsafeRecordConstruct = BuiltIn $ do
                          (s, n) <- popStack2
                          s' <- assertString s
                          n' <- fmap fromInteger $ assertInt n
                          args <- popStack n'
                          let args' = toList $ Stack.FromBottom args
                              qid = splitQualified (Id s')
                          pushStack (Stack.singleton $ makeRecord qid args')

-- This one has a correct type signature at least, but it's still
-- unsafe in that it produces a runtime error if the index is out of
-- bounds. Also, you know, accessing record fields by numerical index
-- is generally a bad idea anyway.

-- ( 'S Any Int -- 'S Any )
unsafeRecordGet :: BuiltIn ()
unsafeRecordGet = BuiltIn $ do
                    (n, obj) <- popStack2
                    n' <- fmap fromInteger $ assertInt n
                    (_, obj') <- assertRecord obj
                    case recordGetField n' obj' of
                      Nothing -> throwError (RuntimeError "record slot index out of bounds")
                      Just x -> pushStack (Stack.singleton x)

-- ( 'R Int Int -- 'R Int )
binmathop :: (Integer -> Integer -> Integer) -> BuiltIn ()
binmathop f = BuiltIn $ popStack2 >>= \(b, a) -> do
                b' <- assertInt b
                a' <- assertInt a
                pushStack (Stack.singleton (Int $ f a' b'))

-- ( 'R Int Int -- 'R Bool )
binmathcmp :: (Integer -> Integer -> Bool) -> BuiltIn ()
binmathcmp f = BuiltIn $ popStack2 >>= \(b, a) -> do
                b' <- assertInt b
                a' <- assertInt a
                pushStack (Stack.singleton (Bool $ f a' b'))

polyFn :: [Type] -> Id -> [Type] -> Id -> BuiltIn () -> ReaderValue
polyFn args arg rets ret =
    let fn = functionType args (RestQuant arg) rets (RestQuant ret)
    in BIFunction (PolyFunctionType (allQuantVars $ FunType fn) fn)

builtins :: Map Id ReaderValue
builtins = Map.fromList [
            ("drop", polyFn [PrimType TAny] "R" [] "R" drop_),
            ("dup", polyFn [QuantVar "a"] "R" [QuantVar "a", QuantVar "a"] "R" dup),
            ("over", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b", QuantVar "a"] "R" over_),
            ("pick", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "c", QuantVar "b", QuantVar "a"] "R" pick),
            ("swap", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b"] "R" swap),
            ("rot", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "c", QuantVar "b"] "R" rot),
            ("dip", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), QuantVar "a"] "S" [QuantVar "a"] "T" dip),
            ("call", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T"))] "S" [] "T" call),
            ("if", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), PrimType TBool] "S" [] "T" if_),
            ("unsafe", polyFn [] "S" [] "T" unsafe),
            ("unsafe1", polyFn [QuantVar "a"] "S" [QuantVar "b"] "S" unsafe1),
            ("unsafe-record-construct", polyFn [PrimType TString, PrimType TInt] "S" [PrimType TAny] "T" unsafeRecordConstruct),
            ("unsafe-record-get", polyFn [PrimType TInt, PrimType TAny] "S" [PrimType TAny] "S" unsafeRecordGet),
            ("+", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (+)),
            ("-", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (-)),
            ("*", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (*)),
            ("/", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop div),
            ("=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (==)),
            ("!=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (/=)),
            ("<", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (<)),
            (">", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (>)),
            ("<=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (<=)),
            (">=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (>=))
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState (Module builtins [] False)

bindPrimitives :: ReadOnlyState -> ReadOnlyState
bindPrimitives =
    over readerNames (Map.insert primitivesModuleName (ModuleValue $ Module builtins [] False))

loadPreludeImpl :: (MonadError FactorError m, MonadIO m) => m ReadOnlyState
loadPreludeImpl = do
  contents <- liftIO $ readFile preludeFileName
  contents' <- liftParseError $ parseManyTokens preludeFileName contents
  decls <- liftParseError $ parseFile preludeFileName contents'
  definednames <- declsToReadOnly (QId []) [ModuleDecl preludeModuleName decls] emptyModule
  let newbindings = ReadOnlyState definednames
      fullbindings = bindPrimitives newbindings
  newbindings' <-
      runReaderT (forOf readerModule newbindings $ resolveAliasesMod Map.empty (QId [])) fullbindings
  let reader'' = bindPrimitives newbindings'
  loadEntities (allNames newbindings') reader''

loadPrelude :: (MonadError FactorError m, MonadIO m) => m Prelude
loadPrelude = fmap Prelude loadPreludeImpl `catchError` (\e -> throwError (InternalError $ show e))

bindStdlibModule :: MonadError FactorError m => Prelude -> ReadOnlyState -> m ReadOnlyState
bindStdlibModule (Prelude p) m = p `merge` m
