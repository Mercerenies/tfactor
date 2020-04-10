{-# LANGUAGE FlexibleContexts #-}

module Factor.Parser.Token(Token(..),
                           semiSpecialChars, specialChars, isSpecialSymbol,
                           token, stringToken, intToken, symbolToken,
                           parseToken, parseManyTokens, satisfy,
                           string, int, symbol,
                           ordinarySymbol, specialSymbol, symbolLiteral) where

import Factor.Id

import Text.Parsec hiding (token, tokens, satisfy, string)
import qualified Text.Parsec as P
import Data.Char
import Control.Monad

type Parser = Parsec String ()

data Token = StringToken SourcePos String
           | IntToken SourcePos Integer
           | SymbolToken SourcePos String
             deriving (Show, Eq)

tokenPos :: Token -> SourcePos
tokenPos (StringToken s _) = s
tokenPos (IntToken s _) = s
tokenPos (SymbolToken s _) = s

semiSpecialChars :: [Char]
semiSpecialChars = ":;()'[]"

specialChars :: [Char]
specialChars = "#\""

isSpecialSymbol :: String -> Bool
isSpecialSymbol [] = True -- Just by default, we'll consider this one special.
isSpecialSymbol xs = head xs `elem` semiSpecialChars || any (`elem` specialChars) xs

spacesOrComments :: Parser ()
spacesOrComments = void $ many (space <|> lineComment)
    where lineComment = char '#' *> many (noneOf "\r\n") *> newline

token :: Parser Token
token = StringToken <$> getPosition <*> stringToken <|>
        IntToken <$> getPosition <*> intToken <|>
        SymbolToken <$> getPosition <*> symbolToken

-- TODO Escape sequences, disallowing newlines, raw strings, etc.,
-- etc.
stringToken :: Parser String
stringToken = char '"' *> many (noneOf ['"']) <* char '"'

sign :: Num a => Parser (a -> a)
sign = id <$ char '+' <|>
       negate <$ char '-'

intToken :: Parser Integer
intToken = try $ option id sign <*> (read <$> many1 digit)

symbolToken :: Parser String
symbolToken = many1 (P.satisfy isValid)
    where isValid ch = not (isSpace ch) && not (isControl ch)

parseToken :: SourceName -> String -> Either ParseError Token
parseToken = parse token

parseManyTokens :: SourceName -> String -> Either ParseError [Token]
parseManyTokens = parse tokens
    where tokens = many (spacesOrComments *> token <* spacesOrComments) <* eof

satisfy :: Stream s m Token => (Token -> Maybe a) -> ParsecT s u m a
satisfy predicate = tokenPrim show nextPos predicate
    where nextPos _ x _ = tokenPos x

string :: Stream s m Token => ParsecT s u m String
string = satisfy go
    where go (StringToken _ s) = Just s
          go _ = Nothing

int :: Stream s m Token => ParsecT s u m Integer
int = satisfy go
    where go (IntToken _ n) = Just n
          go _ = Nothing

symbol :: Stream s m Token => String -> ParsecT s u m Id
symbol s = satisfy go
    where go (SymbolToken _ s') | s == s' = Just $ Id s'
          go _ = Nothing

ordinarySymbol :: Stream s m Token => ParsecT s u m Id
ordinarySymbol = satisfy go
    where go (SymbolToken _ s) | not (isSpecialSymbol s) = Just $ Id s
          go _ = Nothing

specialSymbol :: Stream s m Token => ParsecT s u m Id
specialSymbol = satisfy go
    where go (SymbolToken _ s) | isSpecialSymbol s = Just $ Id s
          go _ = Nothing

symbolLiteral :: Stream s m Token => ParsecT s u m String
symbolLiteral = satisfy go
    where go (SymbolToken _ (':':ss)) = Just ss
          go _ = Nothing
