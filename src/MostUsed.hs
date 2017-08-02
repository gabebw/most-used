module MostUsed
    ( successes
    , failures
    , module MostUsed.Types
    ) where

import Data.Bifunctor
import Data.Either (lefts, rights)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

successes :: Parser [Item] -> String -> [Item]
successes parser s = mconcat $ rights $ successesAndFailures parser $ lines s

failures :: Parser [Item] -> String -> [String]
failures parser s = lefts $ successesAndFailures parser $ lines s

successesAndFailures :: Parser [Item] -> [String] -> [Either String [Item]]
successesAndFailures parser = map (\s -> first parseErrorPretty $ parse parser s s)
