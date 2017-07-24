module Main where

import MostUsed
import Data.List
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
    stdin <- getContents
    print $ findMostUsed $ parseHistory' stdin

findMostUsed :: [Item] -> [(Int, String)]
findMostUsed items = reverseSort $
        map toTuple $
        groupBy sameCommand $
        sortBy (comparing command) items
    where
        toTuple is@(Item c _:_) = (length is, c)
        sameCommand (Item c1 _) (Item c2 _) = c1 == c2

-- Faster than `reverse . sort`:
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (comparing Down)
