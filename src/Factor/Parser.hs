{-# LANGUAGE FlexibleContexts #-}

module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Code.Decl
import Factor.Type hiding (functionType)
import Factor.Stack(Stack(Stack))
import qualified Factor.Stack as Stack
import Factor.Trait.Types
import Factor.Parser.Token

import Text.Parsec hiding (many, (<|>), string, satisfy)
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable
import Control.Lens

type Parser = Parsec [Token] ()

keywords :: [String]
keywords = ["true", "false", "fun", "mod", "end", "macro", "alias", "open", "record",
            "field", "constructor", "val", "require", "include", "type", "of"]

-- (Unused right now)
_id_ :: Parser Id
_id_ = ordinarySymbol

unqualifiedId :: Parser Id
unqualifiedId = try $ do
  Id s <- ordinarySymbol
  when ('.' `elem` s) $ unexpected "qualified identifier"
  return $ Id s

qualifiedId :: Parser QId
qualifiedId = try $ do
  s <- ordinarySymbol
  let QId names = splitQualified s
  when (any (null . unId) names) $ fail "not a valid qualified identifier"
  return $ QId names

nonKeywordQId :: Parser QId
nonKeywordQId = try $ do
  QId xs <- qualifiedId
  case xs of
    [Id x] -> when (x `elem` keywords) $ unexpected $ "keyword " ++ x
    _ -> pure ()
  return $ QId xs

statement :: Parser Statement
statement = Literal <$> literal <|>
            Call <$> nonKeywordQId

functionLit :: Parser Function
functionLit = Function Nothing <$> (symbol "[" *> seq_ <* symbol "]")

literal :: Parser Data
literal = (Int <$> int) <|>
          (Symbol <$> symbolLiteral) <|>
          (Bool True <$ symbol "true") <|>
          (Bool False <$ symbol "false") <|>
          (FunctionValue <$> functionLit <?> "function literal") <|>
          (String <$> string)

seq_ :: Parser Sequence
seq_ = Sequence <$> many statement

decl :: Parser Declaration
decl = (\(t, s) -> FunctionDecl t s) <$> functionDecl <|>
       (\(t, s) -> MacroDecl t s) <$> macroDecl <|>
       (\(i, j) -> ModuleSyn i j) <$> moduleSyn <|>
       (\(i, m) -> ModuleDecl i m) <$> moduleDecl <|>
       (\(i, t) -> TraitDecl i t) <$> trait <|>
       (\(i, d, n) -> RecordDecl i d n) <$> recordDecl decl <|>
       (\(i, vs, f) -> FunctorDecl i vs f) <$> functorDecl <|>
       (\(i, is, t) -> TypeDecl i is t) <$> typeDecl <|>
       (\(i, j) -> AliasDecl i j) <$> aliasDecl <|>
       (\i -> OpenDecl i) <$> openDecl <|>
       (\i -> RequireDecl i) <$> requireDecl <|>
       (\i -> IncludeDecl i) <$> includeDecl

functionDecl :: Parser (PolyFunctionType, Function)
functionDecl = do
  _ <- symbol "fun"
  name <- unqualifiedId
  ty <- functionType
  def <- seq_
  _ <- symbol "end"
  return $ (PolyFunctionType (allQuantVars $ FunType ty) ty, Function (Just name) def)

macroDecl :: Parser (PolyFunctionType, Macro)
macroDecl = do
  _ <- symbol "macro"
  name <- unqualifiedId
  ty <- functionType
  def <- seq_
  _ <- symbol "end"
  return $ (PolyFunctionType (allQuantVars $ FunType ty) ty, Macro name def)

moduleDecl :: Parser (Id, [Declaration])
moduleDecl = do
  name <- try (symbol "mod" *> unqualifiedId <* notFollowedBy (symbol "=" <|> symbol "{"))
  decls <- many decl
  _ <- symbol "end"
  return (name, decls)

moduleSyn :: Parser (Id, Either QId TraitRef)
moduleSyn = do -- TODO Use val not mod here
  name <- try (symbol "mod" *> unqualifiedId <* symbol "=")
  syn <- qualifiedId
  args <- option (Left syn) $ do
              _ <- symbol "{"
              args <- sepBy qualifiedId (symbol ",")
              _ <- symbol "}"
              return (Right $ TraitRef syn args)
  _ <- symbol "end"
  return (name, args)

typeDecl :: Parser (Id, [Id], [TypeInfo])
typeDecl = do
  _ <- symbol "type"
  name <- unqualifiedId
  args <- option [] $ do
             _ <- symbol "{"
             args <- sepBy quantType (symbol ",")
             _ <- symbol "}"
             return args
  body <- many (typeInfo name args)
  return (name, args, body)

typeInfo :: Id -> [Id] -> Parser TypeInfo
typeInfo tname targs = do
  _ <- symbol "|"
  name <- unqualifiedId
  args <- typeInfoGADT tname name targs <|> typeInfoStd tname name targs
  return $ TypeVal name args

typeInfoGADT :: Id -> Id -> [Id] -> Parser (Stack Type)
typeInfoGADT tname name targs = do
  let failure = fail ("Invalid stack effect on val " ++ show name)
  -- We parse a function type then severely restrict it.
  FunctionType (StackDesc args a) (StackDesc rets r) <- functionType
  -- The rest variables must be the same.
  unless (a == r) failure
  case a of { RestQuant _ -> pure () ; _ -> failure } -- If it's a ground term, then wtf?
  -- The result shall consist of exactly one argument: the type name itself
  case rets of { Stack [NamedType (TypeId r' [])] | QId [tname] == r' -> pure () ; _ -> failure }
  -- The arguments can have no variables
  case concatMap allGroundVars (Stack.toList args) ++ concatMap allQuantVars (Stack.toList args) of
    xs | all (`elem` targs) xs -> pure ()
    _ -> failure
  pure args

typeInfoStd :: Id -> Id -> [Id] -> Parser (Stack Type)
typeInfoStd _ name targs = do
  let failure = fail ("Invalid stack effect on val " ++ show name)
  _ <- symbol "of"
  args <- try single <|> multiple
  case concatMap allGroundVars (Stack.toList args) ++ concatMap allQuantVars (Stack.toList args) of
    xs | all (`elem` targs) xs -> pure ()
    _ -> failure
  pure args
      where single = Stack.singleton <$> type_
            multiple = do
              _ <- symbol "("
              args <- many type_
              _ <- symbol ")"
              return (Stack.fromList $ reverse args)

-- An intermediate representation used in the parser for RecordInfo construction
data RecordInfoImpl d = RCon Id | RField Id Type | RDecl d
                        deriving (Show, Eq)

recordDecl :: Parser d -> Parser (Id, [Id], RecordInfo d)
recordDecl declparser = do
  _ <- symbol "record"
  name <- unqualifiedId
  args <- option [] $ do
             _ <- symbol "{"
             args <- sepBy quantType (symbol ",")
             _ <- symbol "}"
             return args
  inner <- many (recordInfo declparser)
  _ <- symbol "end"
  let collate (RCon i) = over _1 (i :)
      collate (RField i t) = over _2 ((i, t) :)
      collate (RDecl d) = over _3 (d :)
      (constrs, fields, decls) = foldl' (flip collate) ([], [], []) inner
  con <- case constrs of
           [con] -> return con
           _ -> fail ("Wrong number of constructors to record " ++ show name)
  return (name, args, RecordInfo con (reverse fields) (reverse decls)) -- Unreversing them from construction order

recordInfo :: Parser d -> Parser (RecordInfoImpl d)
recordInfo declparser =
    RCon <$> (symbol "constructor" *> unqualifiedId) <|>
    RField <$> (symbol "field" *> unqualifiedId) <*> (symbol "of" *> type_) <|>
    RDecl <$> declparser

functorInfo :: Parser (Id, ParamModuleDecl)
functorInfo = (\(t, s) -> (fromJust $ functionName s, PModFunction t s)) <$> functionDecl <|>
              (\(t, s) -> (macroName s, PModMacro t s)) <$> macroDecl <|>
              (\(i, m) -> (i, PModModule m)) <$> modWithinFunctor <|>
              (\(i, t) -> (i, PModTrait t)) <$> trait <|>
              (\(i, a, t) -> (i, PModFunctor a t)) <$> functorWithinFunctor <|>
              (\(i, vs, ts) -> (i, PModType vs ts)) <$> typeDecl

modWithinFunctor :: Parser (Id, Map Id ParamModuleDecl)
modWithinFunctor = do
  name <- try (symbol "mod" *> unqualifiedId <* notFollowedBy (symbol "{"))
  decls <- many functorInfo
  _ <- symbol "end"
  return (name, Map.fromList decls)

functorWithinFunctor :: Parser (Id, [ModuleArg], Map Id ParamModuleDecl)
functorWithinFunctor = do
  name <- try (symbol "mod" *> unqualifiedId <* lookAhead (symbol "{"))
  params <- option [] $ do
              _ <- symbol "{"
              let singleparam = do
                          argname <- unqualifiedId
                          _ <- symbol ":"
                          argtype <- traitRef
                          return $ ModuleArg argname argtype
              params <- sepBy singleparam (symbol ",")
              _ <- symbol "}"
              return params
  info <- Map.fromList <$> many functorInfo
  _ <- symbol "end"
  return (name, params, info)

functorDecl :: Parser (Id, [ModuleArg], Map Id ParamModuleDecl)
functorDecl = do
  name <- try (symbol "mod" *> unqualifiedId <* lookAhead (symbol "{"))
  params <- option [] $ do
              _ <- symbol "{"
              let singleparam = do
                          argname <- unqualifiedId
                          _ <- symbol ":"
                          argtype <- traitRef
                          return $ ModuleArg argname argtype
              params <- sepBy singleparam (symbol ",")
              _ <- symbol "}"
              return params
  info <- Map.fromList <$> many functorInfo
  _ <- symbol "end"
  return (name, params, info)

-- recordFunctorDecl :: Parser (Id, [ModuleArg], [RecordFunInfo])
-- recordFunctorDecl = do
--   name <- try (symbol "record" *> unqualifiedId <* lookAhead (symbol "{"))
--   params <- option [] $ do
--               _ <- symbol "{"
--               let singleparam = do
--                           argname <- unqualifiedId
--                           _ <- symbol ":"
--                           argtype <- traitRef
--                           return $ ModuleArg argname argtype
--               params <- sepBy singleparam (symbol ",")
--               _ <- symbol "}"
--               return params
--   info <- many recordFunInfo
--   _ <- symbol "end"
--   return (name, params, info)

-- recordFunInfo :: Parser RecordFunInfo
-- recordFunInfo =
--     RecordFunConstructor <$> (symbol "constructor" *> unqualifiedId) <|>
--     RecordFunField <$> (symbol "field" *> unqualifiedId) <*> type_ <|>
--     (\(i, f) -> RecordFunOrdinaryDecl i f) <$> functorInfo

aliasDecl :: Parser (Id, QId)
aliasDecl = do
  _ <- symbol "alias"
  name <- unqualifiedId
  _ <- symbol "="
  name' <- qualifiedId
  return (name, name')

openDecl :: Parser QId
openDecl = symbol "open" *> qualifiedId

requireDecl :: Parser TraitRef
requireDecl = symbol "require" *> traitRef

includeDecl :: Parser QId
includeDecl = symbol "include" *> qualifiedId

traitRef :: Parser TraitRef
traitRef = do
  name <- qualifiedId
  args <- option [] $ do
             _ <- symbol "{"
             args <- sepBy qualifiedId (symbol ",")
             _ <- symbol "}"
             return args
  return $ TraitRef name args

trait :: Parser (Id, ParameterizedTrait)
trait = do
  _ <- symbol "trait"
  name <- unqualifiedId
  params <- option [] $ do
              _ <- symbol "{"
              let singleparam = do
                          argname <- unqualifiedId
                          _ <- symbol ":"
                          argtype <- traitRef
                          return $ ModuleArg argname argtype
              params <- sepBy singleparam (symbol ",")
              _ <- symbol "}"
              return params
  properties <- Trait <$> many traitInfo
  _ <- symbol "end"
  return (name, ParameterizedTrait params properties)

traitInfo :: Parser (Id, TraitInfo)
traitInfo = traitInfoFun <|> traitInfoMod <|> -- TODO Macro declarations in traits
            traitInfoInclude <|> traitInfoFunctor <|>
            traitInfoType

traitInfoFun :: Parser (Id, TraitInfo)
traitInfoFun = do
  _ <- symbol "fun"
  name <- unqualifiedId
  ty <- functionType
  return (name, TraitFunction $ PolyFunctionType (allQuantVars $ FunType ty) ty)

traitInfoMod :: Parser (Id, TraitInfo)
traitInfoMod = do
  name <- try (symbol "mod" *> unqualifiedId <* notFollowedBy (symbol "{"))
  inner <- many traitInfo
  _ <- symbol "end"
  return (name, TraitModule inner)

traitInfoFunctor :: Parser (Id, TraitInfo)
traitInfoFunctor = do
  name <- try (symbol "mod" *> unqualifiedId <* lookAhead (symbol "{"))
  params <- option [] $ do
              _ <- symbol "{"
              let singleparam = do
                          argname <- unqualifiedId
                          _ <- symbol ":"
                          argtype <- traitRef
                          return $ ModuleArg argname argtype
              params <- sepBy singleparam (symbol ",")
              _ <- symbol "}"
              return params
  inner <- many traitInfo
  _ <- symbol "end"
  return (name, TraitFunctor params inner)

traitInfoInclude :: Parser (Id, TraitInfo)
traitInfoInclude = do
  _ <- symbol "include"
  name <- traitRef
  return (Id "", TraitInclude name) -- TODO Reorganize Trait so that the empty Id isn't necessary here.

traitInfoType :: Parser (Id, TraitInfo)
traitInfoType = do
  _ <- symbol "type"
  name <- unqualifiedId
  n <- option 0 $ do
             _ <- symbol "{"
             args <- sepBy quantType (symbol ",")
             _ <- symbol "}"
             return (length args)
  return (name, TraitType n)

moduleType :: Parser TypeId
moduleType = do
  qid <- try $ do
           qid <- qualifiedId
           when (qid == QId [Id "--"]) $ unexpected "--"
           return qid
  args <- option [] $ do
               _ <- symbol "{"
               args <- sepBy type_ (symbol ",")
               _ <- symbol "}"
               return args
  return (TypeId qid args)

quantType :: Parser Id
quantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isLower x = Just (Id (x:xs))
          go _ = Nothing

restQuantType :: Parser Id
restQuantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isUpper x = Just (Id (x:xs))
          go _ = Nothing

type_ :: Parser Type
type_ = (NamedType <$> moduleType <?> "named type") <|>
        (QuantVar <$> quantType <?> "type variable") <|>
        (FunType <$> functionType <?> "function type")

stackDesc :: Parser (Either (Stack Type) StackDesc)
stackDesc = go <?> "stack descriptor"
    where go = do
            r <- optionMaybe restQuantType
            args <- many type_
            return $ case r of
                       Nothing -> Left . Stack.fromList $ reverse args
                       Just r' -> Right $ StackDesc (Stack.fromList $ reverse args) (RestQuant r')

functionType :: Parser FunctionType
functionType = do
  _ <- symbol "("
  args <- stackDesc
  _ <- symbol "--"
  rets <- stackDesc
  _ <- symbol ")"
  case (args, rets) of
    (Right args', Right rets') -> return $ FunctionType args' rets'
    (Left args', Left rets') ->
        let used = foldMap allQuantVars (Stack.FromTop args') <>
                   foldMap allQuantVars (Stack.FromTop rets')
            fresh = freshVar "R" used
            args'' = StackDesc args' (RestQuant fresh)
            rets'' = StackDesc rets' (RestQuant fresh)
        in return $ FunctionType args'' rets''
    (_, _) -> fail "stack var asymmetry in function type"

parseStmt :: SourceName -> [Token] -> Either ParseError Statement
parseStmt = parse (statement <* eof)

parseSeq :: SourceName -> [Token] -> Either ParseError Sequence
parseSeq = parse (seq_ <* eof)

parseDecl :: SourceName -> [Token] -> Either ParseError Declaration
parseDecl = parse (decl <* eof)

parseFile :: SourceName -> [Token] -> Either ParseError [Declaration]
parseFile = parse decls
    where decls = many decl <* eof
