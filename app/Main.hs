{-# LANGUAGE NamedFieldPuns #-}
module Main
    ( main
    ) where

import Data.List
import Data.Ord (comparing, Down(..))
import MostUsed as M
import MostUsed.CLI
import Text.Megaparsec.String
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified MostUsed.Parser.Bash as Bash
import qualified MostUsed.Parser.Zsh as Zsh

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
    let rs = results oIncludeFirstArgument $ successes p s
    let stats = prettyPrint $ findMostUsed rs
    putStr $ unlines stats

displayFailures :: Parser [Command] -> String -> IO ()
displayFailures p s = do
    putStrLn "\n!!! The following lines could not be parsed:\n\n"
    putStrLn $ unlines $ failures p s

parser :: Shell -> Parser [Command]
parser Zsh = Zsh.items
parser Bash = Bash.items

prettyPrint :: [Result] -> [String]
prettyPrint = map show

findMostUsed :: [Result] -> [Result]
findMostUsed = reverseSort

reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (comparing Down)

results :: [CommandName] -> [Command] -> [Result]
results includeFirstArgument cs = map (uncurry Result) $
    HM.toList $
    buildMap (withFirstArg includeFirstArgument cs) HM.empty

buildMap :: [String] -> HashMap String Int -> HashMap String Int
buildMap (s:ss) m = buildMap ss $ HM.insertWith (+) s 1 m
buildMap [] m = m

-- Used when including first arg for some commands. Dual-count them so one
-- Command becomes "command" and "command firstArg".
-- Can be slow: O(size(includeFirstArgument) * size(items))
withFirstArg :: [CommandName] -> [Command] -> [String]
withFirstArg [] is = map M.commandName is
withFirstArg _ [] = []
withFirstArg includingFirst (Command n []:is) = n:withFirstArg includingFirst is
withFirstArg includingFirst (Command n (a:_):is) = prefix ++ withFirstArg includingFirst is
    where
        prefix = if n `elem` includingFirst then [n ++ " " ++ show a, n] else [n]
