
module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Type hiding (functionType)
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack
import Factor.Parser.Token

import Text.Parsec hiding (many, (<|>), string, satisfy)
import Data.Char
import Control.Applicative
import Control.Monad

type Parser = Parsec [Token] ()

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

statement :: Parser Statement
statement = Literal <$> literal <|>
            Call <$> qualifiedId

functionLit :: Parser Function
functionLit = Function Nothing <$> (symbol "[" *> seq_ <* symbol "]")

literal :: Parser Data
literal = (Int <$> int) <|>
          (Bool True <$ symbol "true") <|>
          (Bool False <$ symbol "false") <|>
          (FunctionValue <$> functionLit <?> "function literal") <|>
          (String <$> string)

seq_ :: Parser Sequence
seq_ = Sequence <$> many statement

decl :: Parser Declaration
decl = (\(t, s) -> FunctionDecl t s) <$> functionDecl <|>
       (\(i, m) -> ModuleDecl i m) <$> moduleDecl

functionDecl :: Parser (PolyFunctionType, Function)
functionDecl = do
  _ <- symbol ":fun"
  name <- unqualifiedId
  ty <- functionType
  def <- seq_
  _ <- symbol ";"
  return $ (PolyFunctionType (allQuantVars $ FunType ty) ty, Function (Just name) def)

moduleDecl :: Parser (Id, [Declaration])
moduleDecl = do
  _ <- symbol ":mod"
  name <- unqualifiedId
  decls <- many decl
  _ <- symbol ";"
  return (name, decls)

primType :: Parser PrimType
primType = TInt <$ symbol "Int" <|>
           TAny <$ symbol "Any" <|>
           TNothing <$ symbol "Nothing" <|>
           TBool <$ symbol "Bool" <|>
           TString <$ symbol "String"

quantType :: Parser Id
quantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isLower x = Just (Id ('\'':x:xs))
          go _ = Nothing

restQuantType :: Parser Id
restQuantType = satisfy go
    where go (SymbolToken _ ('\'':x:xs)) | isUpper x = Just (Id ('\'':x:xs))
          go _ = Nothing

type_ :: Parser Type
type_ = (PrimType <$> primType <?> "primitive type") <|>
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

freshVar :: [Id] -> Id
freshVar vs = head [v | n <- [0 :: Int ..], let v = Id ("R" <> show n), v `notElem` vs]

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
            fresh = freshVar used
            args'' = StackDesc args' (RestQuant fresh)
            rets'' = StackDesc rets' (RestQuant fresh)
        in return $ FunctionType args'' rets''
    (_, _) -> fail "stack var asymmetry in function type"

parseStmt :: SourceName -> [Token] -> Either ParseError Statement
parseStmt = parse statement

parseSeq :: SourceName -> [Token] -> Either ParseError Sequence
parseSeq = parse seq_

parseDecl :: SourceName -> [Token] -> Either ParseError Declaration
parseDecl = parse decl

parseFile :: SourceName -> [Token] -> Either ParseError [Declaration]
parseFile = parse decls
    where decls = many decl <* eof
