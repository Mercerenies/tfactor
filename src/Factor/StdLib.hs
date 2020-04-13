{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Factor.StdLib(Prelude(),
                     builtins, stdlibs, primitivesModuleName,
                     preludeFileName, preludeModuleName,
                     bindPrimitives, -- TODO This one will be private soon. I need it right now but I won't soon.
                     loadPrelude, bindStdlibModule) where

import Factor.Stack(Stack(..))
import qualified Factor.Stack as Stack
import Factor.State
import Factor.State.Alias
import Factor.Id
import Factor.Type
import Factor.Error
import Factor.Code
import Factor.Eval
import Factor.Loader
import Factor.Parser.Token
import Factor.Parser

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map

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

popStack4 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data)
popStack4 = popStack 4 >>= \case
            Stack [x, y, z, t] -> pure (x, y, z, t)
            _ -> error "Internal error in popStack4"

_popStack5 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data, Data)
_popStack5 = popStack 5 >>= \case
            Stack [x, y, z, t, u] -> pure (x, y, z, t, u)
            _ -> error "Internal error in popStack5"

-- ( 'R Any -- 'R )
drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- ( 'R Any Any -- 'R )
drop2 :: BuiltIn ()
drop2 = BuiltIn $ void (popStack 2)

-- ( 'R Any Any Any -- 'R )
drop3 :: BuiltIn ()
drop3 = BuiltIn $ void (popStack 3)

-- ( 'R Any 'a -- 'R 'a )
nip :: BuiltIn ()
nip = BuiltIn $ popStack2 >>= (\(x, _) -> pushStack (Stack.fromList [x]))

-- ( 'R Any Any 'a -- 'R 'a )
nip2 :: BuiltIn ()
nip2 = BuiltIn $ popStack3 >>= (\(x, _, _) -> pushStack (Stack.fromList [x]))

-- ( 'R Any Any Any 'a -- 'R 'a )
nip3 :: BuiltIn ()
nip3 = BuiltIn $ popStack4 >>= (\(x, _, _, _) -> pushStack (Stack.fromList [x]))

-- ( 'R 'a -- 'R 'a 'a )
dup :: BuiltIn ()
dup = BuiltIn $ popStack1 >>= \x -> pushStack (Stack.fromList [x, x])

-- ( 'R 'a 'b -- 'R 'a 'b 'a 'b )
dup2 :: BuiltIn ()
dup2 = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [x, y, x, y])

-- ( 'R 'a 'b 'c -- 'R 'a 'b 'c 'a 'b 'c )
dup3 :: BuiltIn ()
dup3 = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [x, y, z, x, y, z])

-- ( 'R 'a 'b -- 'R 'a 'b 'a )
over_ :: BuiltIn ()
over_ = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [y, x, y])

-- ( 'R 'a 'b 'c -- 'R 'a 'b 'c 'a 'b )
over2 :: BuiltIn ()
over2 = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [y, z, x, y, z])

-- ( 'R 'a 'b 'c 'd -- 'R 'a 'b 'c 'd 'a 'b 'c )
over3 :: BuiltIn ()
over3 = BuiltIn $ popStack4 >>= \(x, y, z, t) -> pushStack (Stack.fromList [y, z, t, x, y, z, t])

-- This one can be written in the language as simply a no-op. But
-- sometimes explicit is better than implicit.
-- ( 'R -- 'R )
id_ :: BuiltIn ()
id_ = BuiltIn $ pure ()

-- ( 'R 'a 'b -- 'R 'b 'a )
swap :: BuiltIn ()
swap = BuiltIn $ popStack 2 >>= (pushStack . Stack.reverse)

-- ( 'R 'a 'b -- 'R 'a 'a 'b )
dupd :: BuiltIn ()
dupd = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [x, y, y])

-- ( 'R 'a 'b 'c -- 'R 'b 'a 'c )
swapd :: BuiltIn ()
swapd = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [x, z, y])

-- ( 'R 'a 'b 'c -- 'R 'b 'c 'a )
rot :: BuiltIn ()
rot = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [z, x, y])

-- ( 'R 'a 'b 'c -- 'R 'c 'a 'b )
unrot :: BuiltIn ()
unrot = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [y, z, x])

-- ( 'S 'a ( 'S -- 'T ) -- 'T 'a )
dip :: BuiltIn ()
dip = BuiltIn $ popStack2 >>= \(f, x) ->
                assertFunction f >>= \(Function _ ss) ->
                evalSeq ss >> pushStack (Stack.fromList [x])

-- ( 'S 'a 'b ( 'S -- 'T ) -- 'T 'a 'b )
dip2 :: BuiltIn ()
dip2 = BuiltIn $ popStack3 >>= \(f, y, x) ->
                 assertFunction f >>= \(Function _ ss) ->
                 evalSeq ss >> pushStack (Stack.fromList [y, x])

-- ( 'S 'a 'b 'c ( 'S -- 'T ) -- 'T 'a 'b 'c )
dip3 :: BuiltIn ()
dip3 = BuiltIn $ popStack4 >>= \(f, z, y, x) ->
                 assertFunction f >>= \(Function _ ss) ->
                 evalSeq ss >> pushStack (Stack.fromList [z, y, x])

-- ( 'S 'a ( 'S 'a -- 'T ) -- 'T 'a )
keep :: BuiltIn ()
keep = BuiltIn $ popStack2 >>= \(f, x) ->
                 pushStack (Stack.fromList [x]) >>
                 assertFunction f >>= \(Function _ ss) ->
                 evalSeq ss >> pushStack (Stack.fromList [x])

-- ( 'S 'a 'b ( 'S 'a 'b -- 'T ) -- 'T 'a 'b )
keep2 :: BuiltIn ()
keep2 = BuiltIn $ popStack3 >>= \(f, y, x) ->
                 pushStack (Stack.fromList [y, x]) >>
                 assertFunction f >>= \(Function _ ss) ->
                 evalSeq ss >> pushStack (Stack.fromList [y, x])

-- ( 'S 'a 'b 'c ( 'S 'a 'b 'c -- 'T ) -- 'T 'a 'b 'c )
keep3 :: BuiltIn ()
keep3 = BuiltIn $ popStack4 >>= \(f, z, y, x) ->
                 pushStack (Stack.fromList [z, y, x]) >>
                 assertFunction f >>= \(Function _ ss) ->
                 evalSeq ss >> pushStack (Stack.fromList [z, y, x])

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
            ("drop2", polyFn [PrimType TAny, PrimType TAny] "R" [] "R" drop2),
            ("drop3", polyFn [PrimType TAny, PrimType TAny, PrimType TAny] "R" [] "R" drop3),
            ("nip", polyFn [QuantVar "a", PrimType TAny] "R" [QuantVar "a"] "R" nip),
            ("nip2", polyFn [QuantVar "a", PrimType TAny, PrimType TAny] "R" [QuantVar "a"] "R" nip2),
            ("nip3", polyFn [QuantVar "a", PrimType TAny, PrimType TAny, PrimType TAny] "R" [QuantVar "a"] "R" nip3),
            ("dup", polyFn [QuantVar "a"] "R" [QuantVar "a", QuantVar "a"] "R" dup),
            ("dup2", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "b", QuantVar "a"] "R" dup2),
            ("dup3", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "c", QuantVar "b", QuantVar "a", QuantVar "c", QuantVar "b", QuantVar "a"] "R" dup3),
            ("over", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b", QuantVar "a"] "R" over_),
            ("over2", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "c", QuantVar "b", QuantVar "a"] "R" over2),
            ("over3", polyFn [QuantVar "d", QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "c", QuantVar "b", QuantVar "a", QuantVar "d", QuantVar "c", QuantVar "b", QuantVar "a"] "R" over3),
            ("id", polyFn [] "R" [] "R" id_),
            ("swap", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b"] "R" swap),
            ("dupd", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "a"] "R" dupd),
            ("swapd", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "c", QuantVar "a", QuantVar "b"] "R" swapd),
            ("rot", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "c", QuantVar "b"] "R" rot),
            ("unrot", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "c"] "R" unrot),
            ("dip", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), QuantVar "a"] "S" [QuantVar "a"] "T" dip),
            ("dip2", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), QuantVar "b", QuantVar "a"] "S" [QuantVar "b", QuantVar "a"] "T" dip2),
            ("dip3", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), QuantVar "c", QuantVar "b", QuantVar "a"] "S" [QuantVar "c", QuantVar "b", QuantVar "a"] "T" dip3),
            ("keep", polyFn [FunType (functionType [QuantVar "a"] (RestQuant "S") [] (RestQuant "T")), QuantVar "a"] "S" [QuantVar "a"] "T" keep),
            ("keep2", polyFn [FunType (functionType [QuantVar "b", QuantVar "a"] (RestQuant "S") [] (RestQuant "T")), QuantVar "b", QuantVar "a"] "S" [QuantVar "b", QuantVar "a"] "T" keep2),
            ("keep3", polyFn [FunType (functionType [QuantVar "c", QuantVar "b", QuantVar "a"] (RestQuant "S") [] (RestQuant "T")), QuantVar "c", QuantVar "b", QuantVar "a"] "S" [QuantVar "c", QuantVar "b", QuantVar "a"] "T" keep3),
            ("call", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T"))] "S" [] "T" call),
            ("if", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), PrimType TBool] "S" [] "T" if_),
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
stdlibs = ReadOnlyState (Module builtins)

preludeFileName :: FilePath
preludeFileName = "std/Prelude"

preludeModuleName :: Id
preludeModuleName = Id "Prelude"

bindPrimitives :: ReadOnlyState -> ReadOnlyState
bindPrimitives = over readerNames (Map.insert primitivesModuleName (ModuleValue $ Module builtins))

loadPreludeImpl :: (MonadError FactorError m, MonadIO m) => m ReadOnlyState
loadPreludeImpl = do
  contents <- liftIO $ readFile preludeFileName
  contents' <- liftParseError $ parseManyTokens preludeFileName contents
  decls <- liftParseError $ parseFile preludeFileName contents'
  definednames <- declsToReadOnly [ModuleDecl preludeModuleName decls] Map.empty
  let newbindings = ReadOnlyState (Module definednames)
      fullbindings = bindPrimitives newbindings
  aliases <- lookupAndOpenModule (QId [primitivesModuleName]) fullbindings Map.empty
  newbindings' <- forOf readerModule newbindings $ resolveAliasesMod aliases (QId [])
  let reader'' = bindPrimitives newbindings'
  loadEntities (allNames newbindings') reader''

loadPrelude :: (MonadError FactorError m, MonadIO m) => m Prelude
loadPrelude = fmap Prelude loadPreludeImpl `catchError` (\e -> throwError (InternalError $ show e))

primitivesModuleName :: Id
primitivesModuleName = Id "Primitives"

bindStdlibModule :: MonadError FactorError m => Prelude -> ReadOnlyState -> m ReadOnlyState
bindStdlibModule (Prelude p) m = p `merge` m
