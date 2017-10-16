module MostUsed
    ( successes
    , failures
    , module MostUsed.Types
    ) where

import           Data.Bifunctor
import           Data.Either            (lefts, rights)
import           MostUsed.Types
import           Text.Megaparsec
import           Text.Megaparsec.String

successes :: Parser [Command] -> String -> [Command]
successes parser s = mconcat $ rights $ successesAndFailures parser $ lines s

failures :: Parser [Command] -> String -> [String]
failures parser s = lefts $ successesAndFailures parser $ lines s

successesAndFailures :: Parser [Command] -> [String] -> [Either String [Command]]
successesAndFailures parser = map (\s -> first parseErrorPretty $ parse parser s s)
