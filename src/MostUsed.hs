module MostUsed
    ( parseHistory
    , parseHistory'
    , module MostUsed.Types
    ) where

import Data.Char (isSpace, isPrint)
import Data.Either (rights)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

-- Like parseHistory, but just skips over lines that can't be parsed
parseHistory' :: String -> [Item]
parseHistory' s = concat $ rights $ map (parse items "(unknown)") $ lines s

parseHistory :: String -> Either String [Item]
parseHistory s = concat <$> history (lines s)

history :: [String] -> Either String [[Item]]
history (s:ss) = case parse items "(unknown)" s of
                   (Left err) -> Left $ parseErrorPretty err
                   (Right x) -> (:) <$> Right x <*> history ss
history [] = Right []

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
    <|> CommandSubstitution <$> (try (char '$' *> surroundedByParens))
    <|> ProcessSubstitution <$> (char '<' *> surroundedByParens)
    <|> SingleQuoted <$> (char '$' *> surroundedBy "'")
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
