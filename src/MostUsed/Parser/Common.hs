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
    name <- bareWord
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
    <|> CommandSubstitution <$> try (char '$' *> surroundedByParens item)
    <|> Heredoc <$> (string "<<<" *> heredocBody)
    <|> ProcessSubstitution <$> (char '<' *> surroundedByParens item)
    <|> SingleQuoted <$> try (char '$' *> surroundedBy "'")
    <|> NotQuoted <$> bareWord
    <?> "single argument parser"

heredocBody :: Parser String
heredocBody = surroundedBy "'" <|> surroundedBy "\"" <|> bareWord

bareWord :: Parser String
bareWord = some allowedCharsInBareWords

allowedCharsInBareWords :: Parser Char
allowedCharsInBareWords = satisfy (\c -> not (bad c) && isPrint c)
    where
        bad c = c `elem` "|()'\"" || isSpace c

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)
    <?> ("surrounded by " ++ s)

surroundedByParens :: Parser a -> Parser a
surroundedByParens p = char '(' *> p <* char ')'
    <?> "surrounded by parentheses"

escapedNewline :: Parser String
escapedNewline = string "\\n"
