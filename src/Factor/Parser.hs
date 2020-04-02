
module Factor.Parser(parseStmt, parseSeq, parseDecl, parseFile) where

import Factor.Id
import Factor.Code
import Factor.Type
import qualified Factor.Stack as Stack

import Text.Parsec
import Data.Char

type Parser = Parsec String ()

semiSpecialChars :: [Char]
semiSpecialChars = ":;()" -- TBA

specialChars :: [Char]
specialChars = "" -- TBA

spaces1 :: Parser ()
spaces1 = skipMany1 space

id_ :: Parser Id
id_ = Id <$> ((:) <$> (satisfy isStartingChar) <*> many (satisfy isStandardChar))
    where isStartingChar ch = isStandardChar ch && not (isDigit ch) && ch `notElem` semiSpecialChars
          isStandardChar ch = not (isSpace ch) && not (isControl ch) && ch `notElem` specialChars

statement :: Parser Statement
statement = Literal <$> literal <|>
            Call <$> id_

sign :: Num a => Parser (a -> a)
sign = id <$ char '+' <|>
       negate <$ char '-'

literal :: Parser Data
literal = Int <$> (option id sign <*> (read <$> many1 digit))

seq_ :: Parser Sequence
seq_ = Sequence <$> (many $ spaces *> statement <* spaces)

decl :: Parser Declaration
decl = (\(t, s) -> FunctionDecl t s) <$> functionDecl
    where functionDecl = do
            try (char ':' *> spaces1)
            name <- id_ <* spaces
            ty <- functionType <* spaces1
            def <- seq_ <* spaces
            char ';' *> spaces
            return $ (ty, Function (Just name) def)

primType :: Parser PrimType
primType = TInt <$ string "Int" <|>
           TAny <$ string "Any" <|>
           TNothing <$ string "Nothing"

type_ :: Parser Type
type_ = PrimType <$> primType <|>
        FunType <$> functionType

functionType :: Parser FunctionType
functionType = do
  try (char '(' *> spaces1)
  args <- many (type_ <* spaces1)
  string "--" *> spaces
  rets <- many (type_ <* spaces1)
  _ <- char ')'
  -- Reverse since we write the types with stack top on right but
  -- store them with stack top on left.
  return $ FunctionType (Stack.fromList $ reverse args) (Stack.fromList $ reverse rets)

parseStmt :: SourceName -> String -> Either ParseError Statement
parseStmt = parse statement

parseSeq :: SourceName -> String -> Either ParseError Sequence
parseSeq = parse seq_

parseDecl :: SourceName -> String -> Either ParseError Declaration
parseDecl = parse decl

parseFile :: SourceName -> String -> Either ParseError [Declaration]
parseFile = parse decls
    where decls = many (spaces *> decl <* spaces) <* eof
