module Lib
    -- ( evaluate
    -- ) where
    where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (*>), pure)

data Node = Node { expr :: String }

instance Show Node where
    show (Node s) = "Node " ++ (show s)

s = ": 1401927488:0;gcm 'You need jq too'"
ss = ": 1401929181:0;gcm 'Extract methods'\n: 1401929212:0;gcm 'Tell people if they are unranked for a repo'"

evaluate :: String -> Either ParseError [Node]
evaluate = parse parser "(unknown)"

-- A single line :
-- : 1401929181:0;gcm 'Extract methods'
-- A multi-line:
-- : 1401929040:0;gcm 'more tips\
-- \
-- g'

parser :: Parser [Node]
parser = sepBy lineParser endOfLine

newItemStart :: Parser String
newItemStart = string ": "

lineParser :: Parser Node
lineParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    expr <- many (noneOf "\n")
    return $ Node expr

    -- numberNode <- numberNodeParser
    -- spaces
    -- nodes <- many1 furtherExpressionParser
    -- return $ [numberNode] ++ (concat nodes)
