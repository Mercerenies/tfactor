
module Factor.Parser.Token(Token(..),
                           semiSpecialChars, specialChars,
                           token, stringToken, intToken, symbolToken) where

-- Note: Probably won't export the specific parsers later. Just
-- exporting them now for testing.

import Text.Parsec hiding (token)
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
symbolToken = many (satisfy isValid)
    where isValid ch = not (isSpace ch) && not (isControl ch)
