module Lib
    ( Lib.parse
    , test1
    , test2
    , test3
    ) where

import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (*>), pure)

-- An Item is the start (and usually the end) of an item in history.
-- For single-line items, the Item is the whole thing.
-- For multi-line items, the Item contains the first line of the history item,
-- and is followed in the [Node] by an arbitrary number of Extra that each
-- contain a further line of the history item.
-- For example, given this:
--
-- : 1401927488:0;git save
-- : 1401929040:0;gcm 'more
-- whee
-- g'
--
-- The parser would return:
-- [Node "git save", Node "gcm 'more", Extra "whee", Extra "g'"].
data Item = Item { command :: String, arguments :: String }
          | Extra { extra :: String }
          deriving Show

-- 1 single-line history item
test1 :: String
test1 = ": 1401927488:0;gcm 'You need jq too'"

-- 2 single-line history items
test2 :: String
test2 = ": 1401929181:0;gcm 'Extract methods'" ++ "\n" ++
    ": 1401929212:0;gcm 'Mention repo'"

-- 1 single-line history item and 1 multi-line history item
test3 :: String
test3 = ": 1401927488:0;gcm 'Initial commit'" ++ "\n" ++
    ": 1401929040:0;gcm 'more tips" ++ "\n" ++
    "x" ++ "\n" ++
    "g'"

parse :: String -> [Item]
parse = either (const []) gather . evaluate

-- `parse` returns [Item], which may contain Item followed by Extra like
-- [Item, Extra, Extra, Item]. This folds each Extra into the Item before it,
-- which (in this example) would return [Item + Extra + Extra, Item].
--
-- This method and the very existence of Extra is a huge hack that I could
-- probably avoid if I understood `try` or `lookAhead`.
gather :: [Item] -> [Item]
-- Add a newline because by definition, there was a newline between the Item and
-- its Extra.
gather (Item c a:Extra e:is) = gather $ Item c (a ++ "\n" ++ e):is
-- Pass over the first node and keep processing
gather (Item c a:Item c2 a2:is) = (Item c a):gather (Item c2 a2 : is)
-- This shouldn't ever happen
gather ns@(Extra _:_) = error "This shouldn't happen"
-- Done processing
gather is@[Item _ _] = is

evaluate :: String -> Either ParseError [Item]
evaluate = P.parse parser "(unknown)"

parser :: Parser [Item]
parser = sepBy (lineParser <|> extraParser) (char '\n')

newItemStart :: Parser String
newItemStart = string ": "

extraParser :: Parser Item
extraParser = Extra <$> many (noneOf "\n")

lineParser :: Parser Item
lineParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    command <- many1 (noneOf " \t\n")
    spaces
    arguments <- many1 (noneOf "\n")
    return $ Item command arguments
