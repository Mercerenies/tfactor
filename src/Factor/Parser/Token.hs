{-# LANGUAGE FlexibleContexts #-}

module Factor.Parser.Token(Token(..),
                           semiSpecialChars, specialChars, isSpecialSymbol,
                           token, stringToken, intToken, symbolToken,
                           parseToken, parseManyTokens, satisfy) where

import Text.Parsec hiding (token, tokens, satisfy)
import qualified Text.Parsec as P
import Data.Char

type Parser = Parsec String ()

data Token = StringToken String
           | IntToken Integer
           | SymbolToken String
             deriving (Show, Eq)

semiSpecialChars :: [Char]
semiSpecialChars = ":;()'[]\""

specialChars :: [Char]
specialChars = "" -- TBA

isSpecialSymbol :: String -> Bool
isSpecialSymbol [] = True -- Just by default, we'll consider this one special.
isSpecialSymbol xs = head xs `elem` semiSpecialChars || any (`elem` specialChars) xs

token :: Parser Token
token = StringToken <$> stringToken <|>
        IntToken <$> intToken <|>
        SymbolToken <$> symbolToken

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
symbolToken = many (P.satisfy isValid)
    where isValid ch = not (isSpace ch) && not (isControl ch)

parseToken :: SourceName -> String -> Either ParseError Token
parseToken = parse token

parseManyTokens :: SourceName -> String -> Either ParseError [Token]
parseManyTokens = parse tokens
    where tokens = many (spaces *> token <* spaces) <* eof

satisfy :: Stream s m Token => (Token -> Maybe a) -> ParsecT s u m a
satisfy predicate = tokenPrim show nextPos predicate
    where nextPos p _ _ = incSourceColumn p 1 -- TODO
