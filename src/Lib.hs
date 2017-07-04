module Lib
    -- ( evaluate
    -- ) where
    where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (*>), pure)

data Node = Node { expr :: String }
          | Extra { expr :: String } -- extra stuff to be added on later; huge hack

instance Show Node where
    show (Node s) = "Node " ++ (show s)
    show (Extra s) = "Extra " ++ (show s)

s = ": 1401927488:0;gcm 'You need jq too'"
ss = ": 1401929181:0;gcm 'Extract methods'\n: 1401929212:0;gcm 'Tell people if they are unranked for a repo'"
sss = ": 1401927488:0;gcm 'You need jq too'\n: 1401929040:0;gcm 'more tips\nx\ng'"

evaluate :: String -> Either ParseError [Node]
evaluate = parse parser "(unknown)"

parser :: Parser [Node]
parser = do
    sepBy (lineParser <|> extraParser) (char '\n')

newItemStart :: Parser String
newItemStart = string ": "

extraParser :: Parser Node
extraParser = do
    expr <- many (noneOf "\n")
    return $ Extra expr

lineParser :: Parser Node
lineParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    expr <- many (noneOf "\n")
    return $ Node expr
