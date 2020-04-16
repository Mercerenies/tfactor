
module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Type hiding (functionType)
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack
import Factor.Trait.Types
import Factor.Parser.Token

import Text.Parsec hiding (many, (<|>), string, satisfy)
import Data.Char
import Control.Applicative
import Control.Monad

type Parser = Parsec [Token] ()

keywords :: [String]
keywords = ["true", "false", "fun", "mod", "end", "macro", "alias", "open", "field", "constructor", "val"]

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
       (\(i, d) -> RecordDecl i d) <$> recordDecl <|>
       (\(i, j) -> AliasDecl i j) <$> aliasDecl <|>
       (\i -> OpenDecl i) <$> openDecl

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
  name <- try (symbol "mod" *> unqualifiedId <* notFollowedBy (symbol "="))
  decls <- many decl
  _ <- symbol "end"
  return (name, decls)

moduleSyn :: Parser (Id, QId)
moduleSyn = do
  name <- try (symbol "mod" *> unqualifiedId <* symbol "=")
  syn <- qualifiedId
  _ <- symbol "end"
  return (name, syn)

recordDecl :: Parser (Id, [RecordInfo])
recordDecl = do
  _ <- symbol "record"
  name <- unqualifiedId
  decls <- many recordInfo
  _ <- symbol "end"
  return (name, decls)

recordInfo :: Parser RecordInfo
recordInfo =
    RecordConstructor <$> (symbol "constructor" *> unqualifiedId) <|>
    RecordField <$> (symbol "field" *> unqualifiedId) <*> type_ <|>
    RecordOrdinaryDecl <$> decl

aliasDecl :: Parser (Id, QId)
aliasDecl = do
  _ <- symbol "alias"
  name <- unqualifiedId
  _ <- symbol "="
  name' <- qualifiedId
  return (name, name')

openDecl :: Parser QId
openDecl = symbol "open" *> qualifiedId

trait :: Parser (Id, Trait)
trait = symbol "trait" *> ((,) <$> unqualifiedId <*> (Trait <$> many traitInfo)) <* symbol "end"

traitInfo :: Parser (Id, TraitInfo)
traitInfo = traitInfoFun <|> traitInfoMod

traitInfoFun :: Parser (Id, TraitInfo)
traitInfoFun = do
  _ <- symbol "val"
  name <- unqualifiedId
  ty <- functionType
  return (name, TraitFunction $ PolyFunctionType (allQuantVars $ FunType ty) ty)

traitInfoMod :: Parser (Id, TraitInfo)
traitInfoMod = do
  _ <- symbol "mod"
  name <- unqualifiedId
  inner <- many traitInfo
  _ <- symbol "end"
  return (name, TraitModule inner)

namedType :: Parser QId
namedType = try $ do
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
type_ = (NamedType <$> namedType <?> "named type") <|>
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
