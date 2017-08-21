module MostUsed.Parser.Common
    ( item
    , pipe
    ) where

import Data.Char (isSpace, isPrint)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

item :: Parser Command
item = do
    name <- some allowedCharsInBareWords
    space
    args <- singleArgument `sepEndBy` separator
    return $ Command name args

pipe :: Parser ()
pipe = space >> char '|' >> space

separator :: Parser [String]
separator = some (try escapedNewline <|> some spaceChar)

singleArgument :: Parser Argument
singleArgument =
    DoubleQuoted <$> surroundedBy "\""
    <|> SingleQuoted <$> surroundedBy "'"
    <|> Backticks <$> surroundedBy "`"
    <|> CommandSubstitution <$> try (string "$(" *> item <* char ')')
    <|> Heredoc <$> try (string "<<<" *> heredocBody)
    <|> ProcessSubstitution <$> (char '<' *> surroundedByParens)
    <|> SingleQuoted <$> try (char '$' *> surroundedBy "'")
    <|> NotQuoted <$> some allowedCharsInBareWords
    <?> "single argument parser"

heredocBody :: Parser String
heredocBody =
    surroundedBy "'"
    <|> surroundedBy "\""
    <|> some allowedCharsInBareWords

allowedCharsInBareWords :: Parser Char
allowedCharsInBareWords = satisfy (\c -> not (bad c) && isPrint c)
    where
        bad c = c `elem` "|()'\"" || isSpace c

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)
    <?> ("surrounded by " ++ s)

surroundedByParens :: Parser String
surroundedByParens = between (char '(') (char ')') (many $ noneOf "()")
    <?> "surrounded by parentheses"

escapedNewline :: Parser String
escapedNewline = string "\\n"
