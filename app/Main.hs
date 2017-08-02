{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.List
import Data.Ord (comparing, Down(..))
import MostUsed as M
import MostUsed.Parser.Zsh as Zsh
import MostUsed.CLI

main :: IO ()
main = do
    Options{oIncludeFirstArgument, oDebug} <- parseCLI
    stdinContents <- getContents
    let f = if oDebug
            then displayFailures
            else displaySuccesses oIncludeFirstArgument
    f stdinContents

displaySuccesses :: [Command] -> String -> IO ()
displaySuccesses oIncludeFirstArgument s = do
    let results = successes Zsh.items s
    let stats = prettyPrint $ findMostUsed oIncludeFirstArgument results
    putStr $ unlines stats

displayFailures :: String -> IO ()
displayFailures s = do
    putStrLn "\n!!! The following lines could not be parsed:\n\n"
    putStrLn $ unlines $ failures Zsh.items s

prettyPrint :: [(Int, String)] -> [String]
prettyPrint stats = map (\(n, count) -> show n ++ " " ++ count) stats

findMostUsed :: [Command] -> [Item] -> [(Int, String)]
findMostUsed includeFirstArgument items = reverseSort $
        map toTuple $
        group $
        sort $
        withFirstArg includeFirstArgument items
    where
        toTuple xs@(x:_) = (length xs, x)

-- Used when including first arg for some items. Dual-count them so one Item
-- becomes "command" and "command firstArg".
-- Can be slow: O(size(includeFirstArgument) * size(items))
withFirstArg :: [Command] -> [Item] -> [String]
withFirstArg [] is = map M.command is
withFirstArg _ [] = []
withFirstArg includingFirst (Item c []:is) = c:withFirstArg includingFirst is
withFirstArg includingFirst (Item c (a:_):is) = prefix ++ withFirstArg includingFirst is
    where
        prefix = if c `elem` includingFirst then [c ++ " " ++ show a, c] else [c]

-- Faster than `reverse . sort`:
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (comparing Down)
