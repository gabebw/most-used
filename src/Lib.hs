module Lib
    ( Lib.parse
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
data Item = Item { expr :: String }
          | Extra { expr :: String }

instance Show Item where
    show (Item s) = "Item " ++ show s
    show (Extra s) = "Extra " ++ show s

s = ": 1401927488:0;gcm 'You need jq too'"
ss = ": 1401929181:0;gcm 'Extract methods'\n: 1401929212:0;gcm 'Tell people if they are unranked for a repo'"
sss = ": 1401927488:0;gcm 'You need jq too'\n: 1401929040:0;gcm 'more tips\nx\ng'"

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
gather (Item n:Extra e:ns) = gather $ Item (n ++ "\n" ++ e):ns
-- Pass over the first node and keep processing
gather (Item n:Item o:ns) = Item n:gather (Item o : ns)
-- This shouldn't ever happen
gather ns@(Extra e:_) = gather ns
-- Done processing
gather ns@[Item _] = ns

evaluate :: String -> Either ParseError [Item]
evaluate = P.parse parser "(unknown)"

parser :: Parser [Item]
parser = sepBy (lineParser <|> extraParser) (char '\n')

newItemStart :: Parser String
newItemStart = string ": "

extraParser :: Parser Item
extraParser = do
    expr <- many (noneOf "\n")
    return $ Extra expr

lineParser :: Parser Item
lineParser = do
    newItemStart
    many1 digit
    char ':'
    many1 digit
    char ';'
    expr <- many (noneOf "\n")
    return $ Item expr
