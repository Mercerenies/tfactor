
module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Code.Decl
import Factor.Type hiding (functionType)
import Factor.Stack(Stack)
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

type Parser = Parsec [Token] ()

keywords :: [String]
keywords = ["true", "false", "fun", "mod", "end", "macro", "alias", "open",
            "field", "constructor", "val", "require", "include"]

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
--       (\(i, d) -> RecordDecl i d) <$> recordDecl <|>
       (\(i, f) -> FunctorDecl i f) <$> functorDecl <|>
--       (\(i, a, d) -> RecordFunctorDecl i a d) <$> recordFunctorDecl <|>
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
moduleSyn = do
  name <- try (symbol "mod" *> unqualifiedId <* symbol "=")
  syn <- qualifiedId
  args <- option (Left syn) $ do
              _ <- symbol "{"
              args <- sepBy qualifiedId (symbol ",")
              _ <- symbol "}"
              return (Right $ TraitRef syn args)
  _ <- symbol "end"
  return (name, args)

-- recordDecl :: Parser (Id, [RecordInfo])
-- recordDecl = do
--   name <- try (symbol "record" *> unqualifiedId <* notFollowedBy (symbol "{"))
--   decls <- many recordInfo
--   _ <- symbol "end"
--   return (name, decls)

-- recordInfo :: Parser RecordInfo
-- recordInfo =
--     RecordConstructor <$> (symbol "constructor" *> unqualifiedId) <|>
--     RecordField <$> (symbol "field" *> unqualifiedId) <*> type_ <|>
--     RecordOrdinaryDecl <$> decl

functorInfo :: Parser (Id, FunctorInfo)
functorInfo = (\(t, s) -> (fromJust $ functionName s, FunctorUDFunction t s)) <$> functionDecl <|>
              (\(t, s) -> (macroName s, FunctorUDMacro t s)) <$> macroDecl <|>
              (\(i, m) -> (i, FunctorModule m)) <$> modWithinFunctor <|>
              (\(i, t) -> (i, FunctorTrait t)) <$> trait <|>
              (\(i, a, t) -> (i, FunctorFunctor a t)) <$> functorWithinFunctor

modWithinFunctor :: Parser (Id, Map Id FunctorInfo)
modWithinFunctor = do
  name <- try (symbol "mod" *> unqualifiedId <* notFollowedBy (symbol "{"))
  decls <- many functorInfo
  _ <- symbol "end"
  return (name, Map.fromList decls)

functorWithinFunctor :: Parser (Id, [ModuleArg], Map Id FunctorInfo)
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

functorDecl :: Parser (Id, ParameterizedModule)
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
  return (name, ParameterizedModule params info)

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
            traitInfoInclude <|> traitInfoFunctor

traitInfoFun :: Parser (Id, TraitInfo)
traitInfoFun = do
  _ <- symbol "val"
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

moduleType :: Parser QId
moduleType = try $ do
  qid <- qualifiedId
  when (qid == QId [Id "--"]) $ unexpected "--"
  return qid

quantType :: Parser Id
quantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isLower x = Just (Id (x:xs))
          go _ = Nothing

restQuantType :: Parser Id
restQuantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isUpper x = Just (Id (x:xs))
          go _ = Nothing

type_ :: Parser Type
type_ = (ModuleType <$> moduleType <?> "named type") <|>
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
