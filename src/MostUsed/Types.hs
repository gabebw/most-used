module MostUsed.Types
    ( CommandName
    , Argument(..)
    , Command(..)
    , Shell(..)
    ) where

import Data.List (sortBy)
import Data.Ord (comparing)

type CommandName = String

data Shell = Zsh | Bash

data Command = Command { commandName :: CommandName, arguments :: [Argument] }
            deriving (Show, Eq)

data Result = Result
    -- Has to be a commandName b/c almost no commands with all their arguments
    -- will be dupes.
    { resultName :: CommandName
    , frequency :: Int
    }

value :: (Num a) => Result -> a
value (Result n f) = fromIntegral $ length n * f

-- Get value of nth percentile of the list
percentileBoundary :: Int -> [a] -> Float
percentileBoundary n xs = (fromIntegral ((length xs) * n)) / 100 + 0.5

-- Get top nth percentile of a list
topPercentile :: Int -> [Result] -> [Result]
topPercentile n xs = filter (\r -> value r >= p) sorted
    where
        p = percentileBoundary n (map value xs)
        sorted = sortBy (comparing value) xs

data Argument = DoubleQuoted String
              | SingleQuoted String
              | NotQuoted String
              | Backticks String
              | CommandSubstitution String
              | ProcessSubstitution String
              deriving (Eq)

instance Show Argument where
    show (DoubleQuoted s) = "\"" ++ s ++ "\""
    show (SingleQuoted s) = "'" ++ s ++ "'"
    show (NotQuoted s) = s
    show (Backticks s) = "`" ++ s ++ "`"
    show (CommandSubstitution s) = "$(" ++ s ++ ")"
    show (ProcessSubstitution s) = "<(" ++ s ++ ")"
