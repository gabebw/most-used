module MostUsed.CLI.Display
    ( failedParses
    , successfulParses
    ) where

import qualified Data.HashMap.Strict    as HM
import           Data.List              (sortBy)
import           Data.Ord               (Down (..), comparing)
import           MostUsed               (failures, successes)
import           MostUsed.CLI.Util      (buildMap)
import           MostUsed.Types         as M
import           Text.Megaparsec.String (Parser)

successfulParses :: [CommandName] -> Parser [Command] -> String -> String
successfulParses includeFirstArgument p s = unlines stats
    where
        stats = prettyPrint $ findMostUsed includeFirstArgument results
        results = successes p s

failedParses :: Parser [Command] -> String -> String
failedParses p s = unlines $ warning : failures p s
    where
        warning = "\n!!! The following lines could not be parsed:\n\n"

prettyPrint :: [(String, Int)] -> [String]
prettyPrint stats = map (\(count, n) -> show n ++ " " ++ count) stats

findMostUsed :: [CommandName] -> [Command] -> [(String, Int)]
findMostUsed includeFirstArgument items = sortBy (comparing (Down . snd)) $
    HM.toList $
    buildMap (withFirstArg includeFirstArgument items)

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
