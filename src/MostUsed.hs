module MostUsed
    ( parseHistory
    , Argument(..)
    , Item(..)
    ) where

import Data.Char (isSpace, isPrint)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

data Item = Item { command :: String, arguments :: [Argument] }
            deriving (Show, Eq)

data Argument = DoubleQuoted String
              | SingleQuoted String
              | NotQuoted String
              | Backticks String
              | CommandSubstitution String
              deriving (Show, Eq)

parseHistory :: String -> Either String [Item]
parseHistory s = history $ lines s

history :: [String] -> Either String [Item]
history (s:ss) = case parse itemParser "(unknown)" s of
                        (Left err) -> Left $ parseErrorPretty err
                        (Right x) -> (:) <$> Right x <*> history ss
history [] = Right []

itemParser :: Parser Item
itemParser = do
    some spaceChar
    some digitChar -- command number
    string "  "
    command <- some (satisfy (not . isSpace))
    space
    arguments <- argumentsParser
    eof
    return $ Item command arguments

argumentsParser :: Parser [Argument]
argumentsParser = singleArgumentParser `sepBy` separator

separator :: Parser [String]
separator = some (try escapedNewline <|> some spaceChar)

singleArgumentParser :: Parser Argument
singleArgumentParser =
    DoubleQuoted <$> surroundedBy "\""
    <|> SingleQuoted <$> surroundedBy "'"
    <|> Backticks <$> surroundedBy "`"
    <|> CommandSubstitution <$> (char '$' *> surroundedByParens)
    <|> NotQuoted <$> some allowedCharsInArguments
    <?> "single argument parser"

allowedCharsInArguments :: Parser Char
allowedCharsInArguments = satisfy (\c -> not (isSpace c) && isPrint c)

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)
    <?> ("surrounded by " ++ s)

surroundedByParens :: Parser String
surroundedByParens = between (char '(') (char ')') (many $ noneOf "()")
    <?> "surrounded by parentheses"

escapedNewline :: Parser String
escapedNewline = string "\\n"
