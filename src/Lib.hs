module Lib
    ( parseHistory
    , test1
    , test2
    , test3
    ) where

import Control.Monad (void)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Item = Item { command :: String, arguments :: String }
          deriving Show

-- 1 single-line history item
test1 :: String
test1 = ": 1401927488:0;gcm 'You need jq too'"

-- 2 single-line history items
test2 :: String
test2 = ": 1401929181:0;gcm 'Extract methods'" ++ "\n" ++
    ": 1401929212:0;ls"

-- 1 single-line history item and 1 multi-line history item
test3 :: String
test3 = ": 1401927488:0;gcm 'Initial commit'" ++ "\n" ++
    ": 1401929040:0;gcm 'more tips" ++ "\n" ++
    "x" ++ "\n" ++
    "g'"

parseHistory :: String -> [Item]
parseHistory = either (const []) id . evaluate

evaluate :: String -> Either ParseError [Item]
evaluate = parse parser "(unknown)"

parser :: Parser [Item]
parser = many lineParser

lineParser :: Parser Item
lineParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    command <- many1 (noneOf " \t\n")
    spaces
    arguments <- manyTill anyChar endOfItem
    return $ Item command arguments

newItemStart :: Parser String
newItemStart = string ": "

-- lookAhead looks for (but does not consume) the start of the next item.
-- If there's no next item, we should be at the end of the file.
endOfItem :: Parser ()
endOfItem = void (lookAhead newItemStart) <|> eof
