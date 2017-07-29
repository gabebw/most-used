module MostUsed
    ( parseHistory
    , parseHistory'
    , Command(..)
    , Argument(..)
    , Item(..)
    ) where

import Data.Char (isSpace, isPrint)
import Data.Either (rights)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

type Command = String

data Item = Item { command :: Command, arguments :: [Argument] }
            deriving (Show, Eq)

data Argument = DoubleQuoted String
              | SingleQuoted String
              | NotQuoted String
              | Backticks String
              | CommandSubstitution String
              | ProcessSubstitution String
              deriving (Eq)

instance Show Argument where
    show (DoubleQuoted s) = "\"" ++ s ++ "\""
    show (SingleQuoted s) = "'" ++ s ++ "'"
    show (NotQuoted s) = s
    show (Backticks s) = "`" ++ s ++ "`"
    show (CommandSubstitution s) = "$(" ++ s ++ ")"
    show (ProcessSubstitution s) = "<(" ++ s ++ ")"

-- Like parseHistory, but just skips over lines that can't be parsed
parseHistory' :: String -> [Item]
parseHistory' s = concat $ rights $ map (parse lineParser "(unknown)") $ lines s

parseHistory :: String -> Either String [Item]
parseHistory s = concat <$> history (lines s)

history :: [String] -> Either String [[Item]]
history (s:ss) = case parse lineParser "(unknown)" s of
                   (Left err) -> Left $ parseErrorPretty err
                   (Right x) -> (:) <$> Right x <*> history ss
history [] = Right []

lineParser :: Parser [Item]
lineParser = do
    some spaceChar
    some digitChar -- history number
    string "  "
    items <- itemParser `sepBy` pipeParser
    eof
    return items

pipeParser :: Parser ()
pipeParser = space >> char '|' >> space

itemParser :: Parser Item
itemParser = do
    command <- some (satisfy (not . isSpace))
    space
    arguments <- argumentsParser
    return $ Item command arguments

argumentsParser :: Parser [Argument]
argumentsParser = singleArgumentParser `sepEndBy` separator

separator :: Parser [String]
separator = some (try escapedNewline <|> some spaceChar)

singleArgumentParser :: Parser Argument
singleArgumentParser =
    DoubleQuoted <$> surroundedBy "\""
    <|> SingleQuoted <$> surroundedBy "'"
    <|> Backticks <$> surroundedBy "`"
    <|> CommandSubstitution <$> (char '$' *> surroundedByParens)
    <|> ProcessSubstitution <$> (char '<' *> surroundedByParens)
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
