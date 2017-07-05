module MostUsed
    ( parseHistory
    , Argument(..)
    , Item(..)
    ) where

import Data.Char
import Control.Monad (void)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Item = Item { command :: String, arguments :: [Argument] }
            deriving (Show, Eq)

data Argument = DoubleQuoted String
              | SingleQuoted String
              | NotQuoted String
              -- Delete Backticks and make it CommandSubstitution too?
              | Backticks String
              | CommandSubstitution String
              deriving (Show, Eq)

parseHistory :: String -> [Item]
parseHistory = either (const []) id . evaluate

evaluate :: String -> Either ParseError [Item]
evaluate = parse parser "(unknown)"

parser :: Parser [Item]
parser = many itemParser

{-
Example history items:

: 1401927488:0;gcm 'You need jq too'
: 1401929040:0;gcm 'more tips\
\
g'
: 1401929181:0;gcm 'Extract methods'
-}
itemParser :: Parser Item
itemParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    command <- many1 notSpace
    spaces
    arguments <- manyTill separatedArgumentsParser endOfItem
    return $ Item command arguments

separatedArgumentsParser :: Parser Argument
separatedArgumentsParser = singleArgumentParser <* many space

notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

singleArgumentParser :: Parser Argument
singleArgumentParser = do
    (DoubleQuoted <$> surroundedBy "\"")
    <|> (SingleQuoted <$> surroundedBy "'")
    <|> (Backticks <$> surroundedBy "`")
    <|> (CommandSubstitution <$> (char '$' *> surroundedByParens))
    <|> (NotQuoted <$> many1 allowedCharsInArguments)

allowedCharsInArguments :: Parser Char
allowedCharsInArguments = satisfy (\c -> not (isSpace c) && isPrint c)

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)

surroundedByParens :: Parser String
surroundedByParens = between (char '(') (char ')') (many $ noneOf "()")

newItemStart :: Parser String
newItemStart = string ": "

-- lookAhead looks for (but does not consume) the start of the next item.
-- If there's no next item, we should be at the end of the file.
endOfItem :: Parser ()
endOfItem = void (lookAhead newItemStart) <|> eof
