module MostUsed.Parser.Common
    ( item
    , pipe
    ) where

import Data.Char (isSpace, isPrint)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

item :: Parser Item
item = do
    command <- some allowedCharsInBareWords
    space
    arguments <- singleArgument `sepEndBy` separator
    return $ Item command arguments

pipe :: Parser ()
pipe = space >> char '|' >> space

separator :: Parser [String]
separator = some (try escapedNewline <|> some spaceChar)

singleArgument :: Parser Argument
singleArgument =
    DoubleQuoted <$> surroundedBy "\""
    <|> SingleQuoted <$> surroundedBy "'"
    <|> Backticks <$> surroundedBy "`"
    <|> CommandSubstitution <$> try (char '$' *> surroundedByParens)
    <|> ProcessSubstitution <$> (char '<' *> surroundedByParens)
    <|> SingleQuoted <$> try (char '$' *> surroundedBy "'")
    <|> NotQuoted <$> some allowedCharsInBareWords
    <?> "single argument parser"

allowedCharsInBareWords :: Parser Char
allowedCharsInBareWords = satisfy (\c ->
    c /= '|' && not (isSpace c) && isPrint c)

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)
    <?> ("surrounded by " ++ s)

surroundedByParens :: Parser String
surroundedByParens = between (char '(') (char ')') (many $ noneOf "()")
    <?> "surrounded by parentheses"

escapedNewline :: Parser String
escapedNewline = string "\\n"
