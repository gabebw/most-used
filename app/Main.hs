{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    ( main
    ) where

import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.List
import           Data.Ord               (Down (..), comparing)
import           MostUsed
import           MostUsed.CLI
import qualified MostUsed.Parser.Bash   as Bash
import qualified MostUsed.Parser.Zsh    as Zsh
import           MostUsed.Types         as M
import           Text.Megaparsec.String

main :: IO ()
main = do
    Options{..} <- parseCLI
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
parser Zsh  = Zsh.items
parser Bash = Bash.items

prettyPrint :: [(String, Int)] -> [String]
prettyPrint stats = map (\(count, n) -> show n ++ " " ++ count) stats

findMostUsed :: [CommandName] -> [Command] -> [(String, Int)]
findMostUsed includeFirstArgument items = sortBy (comparing (Down . snd)) $
    HM.toList $
    buildMap (withFirstArg includeFirstArgument items) HM.empty

buildMap :: [String] -> HashMap String Int -> HashMap String Int
buildMap (s:ss) m = buildMap ss $ HM.insertWith (+) s 1 m
buildMap [] m     = m

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
