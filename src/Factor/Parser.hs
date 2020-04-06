
module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Type hiding (functionType)
import qualified Factor.Stack as Stack

import Text.Parsec
import Data.Char
import Data.Maybe(listToMaybe)
import Control.Monad

type Parser = Parsec String ()

semiSpecialChars :: [Char]
semiSpecialChars = ":;()'[]"

specialChars :: [Char]
specialChars = "" -- TBA

spaces1 :: Parser ()
spaces1 = skipMany1 space

id_ :: Parser Id
id_ = Id <$> ((:) <$> (satisfy isStartingChar) <*> many (satisfy isStandardChar)) <?> "identifier"
    where isStartingChar ch = isStandardChar ch && not (isDigit ch) && ch `notElem` semiSpecialChars
          isStandardChar ch = not (isSpace ch) && not (isControl ch) && ch `notElem` specialChars

lowerId :: Parser Id
lowerId = go <?> "lowercase identifier"
    where go = do
            Id i <- id_
            unless (maybe False isLower (listToMaybe i))
                   (unexpected "identifier" <?> "lowercase identifier")
            return (Id i)

upperId :: Parser Id
upperId = go <?> "uppercase identifier"
    where go = do
            Id i <- id_
            unless (maybe False isUpper (listToMaybe i))
                   (unexpected "identifier" <?> "uppercase identifier")
            return (Id i)

statement :: Parser Statement
statement = Literal <$> literal <|>
            Call <$> id_

sign :: Num a => Parser (a -> a)
sign = id <$ char '+' <|>
       negate <$ char '-'

functionLit :: Parser Function
functionLit = Function Nothing <$> (char '[' *> spaces *> seq_ <* spaces <* char ']')

literal :: Parser Data
literal = (Int <$> (try (option id sign <*> (read <$> many1 digit)) <?> "integer literal")) <|>
          (FunctionValue <$> functionLit <?> "function literal")

seq_ :: Parser Sequence
seq_ = Sequence <$> (many . try $ spaces *> statement <* spaces)

decl :: Parser Declaration
decl = (\(t, s) -> FunctionDecl t s) <$> functionDecl
    where functionDecl = do
            try (char ':' *> spaces1)
            name <- id_ <* spaces
            ty <- functionType <* spaces1
            def <- seq_ <* spaces
            char ';' *> spaces
            return $ (PolyFunctionType (allQuantVars $ FunType ty) ty, Function (Just name) def)

primType :: Parser PrimType
primType = TInt <$ string "Int" <|>
           TAny <$ string "Any" <|>
           TNothing <$ string "Nothing" <|>
           TBool <$ string "Bool"

quantType :: Parser Id
quantType = char '\'' *> lowerId

type_ :: Parser Type
type_ = (PrimType <$> primType <?> "primitive type") <|>
        (QuantVar <$> quantType <?> "type variable") <|>
        (FunType <$> functionType <?> "function type")

stackDesc :: Parser StackDesc
stackDesc = go <?> "stack descriptor"
    where go = do
            r <- char '\'' *> upperId
            args <- many $ try (spaces1 *> type_)
            return $ StackDesc (Stack.fromList $ reverse args) (RestQuant r)

-- TODO Make the stack rest var passthrough if it's not listed (right
-- now, it's required)
functionType :: Parser FunctionType
functionType = do
  try (char '(' *> spaces1)
  args <- stackDesc
  spaces *> string "--" *> spaces
  rets <- stackDesc
  _ <- spaces *> char ')'
  return $ FunctionType args rets

parseStmt :: SourceName -> String -> Either ParseError Statement
parseStmt = parse statement

parseSeq :: SourceName -> String -> Either ParseError Sequence
parseSeq = parse seq_

parseDecl :: SourceName -> String -> Either ParseError Declaration
parseDecl = parse decl

parseFile :: SourceName -> String -> Either ParseError [Declaration]
parseFile = parse decls
    where decls = many (spaces *> decl <* spaces) <* eof
