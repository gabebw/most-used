module MostUsed.Parser.Common
    ( items
    ) where

import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isPrint)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = item `sepByAnyOf` commandSeparators <* eof
    where
        commandSeparators = map surroundedBySpace ['|', ';']
        surroundedBySpace :: Char -> Parser ()
        surroundedBySpace c = space >> char c >> space

-- This is exactly like `sepBy`, but with multiple possible separators.
sepByAnyOf :: Alternative m => m a -> [m b] -> m [a]
sepByAnyOf p seps = (:) <$> p <*> many (choice (map (*> p) seps))

item :: Parser Command
item = do
    name <- bareWord
    space
    args <- singleArgument `sepEndBy` argumentSeparator
    return $ Command name args

argumentSeparator :: Parser [String]
argumentSeparator = some (try escapedNewline <|> some spaceChar)

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
