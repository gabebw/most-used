module MostUsed
    ( successes
    , failures
    , module MostUsed.Types
    ) where

import Data.Bifunctor
import Data.Char (isSpace, isPrint)
import Data.Either (lefts, rights)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

successes :: String -> [Item]
successes = mconcat . rights . successesAndFailures . lines

failures :: String -> [String]
failures = lefts . successesAndFailures . lines

successesAndFailures :: [String] -> [Either String [Item]]
successesAndFailures = map (\s -> first parseErrorPretty $ parse items s s)

items :: Parser [Item]
items = do
    some spaceChar
    some digitChar -- history number
    string "  "
    item `sepBy` pipe <* eof

pipe :: Parser ()
pipe = space >> char '|' >> space

item :: Parser Item
item = do
    command <- some $ satisfy (not . isSpace)
    space
    arguments <- singleArgument `sepEndBy` separator
    return $ Item command arguments

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
    <|> NotQuoted <$> some allowedCharsInArguments
    <?> "single argument parser"

allowedCharsInArguments :: Parser Char
allowedCharsInArguments = satisfy (\c ->
    c /= '|' && not (isSpace c) && isPrint c)

surroundedBy :: String -> Parser String
surroundedBy s = between (string s) (string s) (many $ noneOf s)
    <?> ("surrounded by " ++ s)

surroundedByParens :: Parser String
surroundedByParens = between (char '(') (char ')') (many $ noneOf "()")
    <?> "surrounded by parentheses"

escapedNewline :: Parser String
escapedNewline = string "\\n"
