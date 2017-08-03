{-# LANGUAGE NamedFieldPuns #-}
module Main
    ( main
    ) where

import Data.List
import Data.Maybe
import Data.Ord (comparing, Down(..))
import MostUsed as M
import MostUsed.CLI
import MostUsed.Parser.Bash as Bash
import MostUsed.Parser.Zsh as Zsh
import System.Environment
import Text.Megaparsec.String

main :: IO ()
main = do
    Options{oIncludeFirstArgument, oDebug, oShell} <- parseCLI
    stdinContents <- getContents
    let f = if oDebug
            then displayFailures
            else displaySuccesses oIncludeFirstArgument
    f (parser oShell) stdinContents

displaySuccesses :: [CommandName] -> Parser [Command] -> String -> IO ()
displaySuccesses oIncludeFirstArgument p s = do
    let results = successes p s
    let stats = prettyPrint $ findMostUsed oIncludeFirstArgument results
    putStr $ unlines stats

displayFailures :: Parser [Command] -> String -> IO ()
displayFailures p s = do
    putStrLn "\n!!! The following lines could not be parsed:\n\n"
    putStrLn $ unlines $ failures p s

parser :: Shell -> Parser [Command]
parser Zsh = Zsh.items
parser Bash = Bash.items

prettyPrint :: [(Int, String)] -> [String]
prettyPrint stats = map (\(n, count) -> show n ++ " " ++ count) stats

findMostUsed :: [CommandName] -> [Command] -> [(Int, String)]
findMostUsed includeFirstArgument items = reverseSort $
        map toTuple $
        group $
        sort $
        withFirstArg includeFirstArgument items
    where
        toTuple xs@(x:_) = (length xs, x)

-- Used when including first arg for some items. Dual-count them so one Command
-- becomes "command" and "command firstArg".
-- Can be slow: O(size(includeFirstArgument) * size(items))
withFirstArg :: [CommandName] -> [Command] -> [String]
withFirstArg [] is = map M.commandName is
withFirstArg _ [] = []
withFirstArg includingFirst (Command n []:is) = n:withFirstArg includingFirst is
withFirstArg includingFirst (Command n (a:_):is) = prefix ++ withFirstArg includingFirst is
    where
        prefix = if n `elem` includingFirst then [n ++ " " ++ show a, n] else [n]

-- Faster than `reverse . sort`:
-- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (comparing Down)
