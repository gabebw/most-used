module MostUsed.CLI.Util
    ( buildMap
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

buildMap :: [String] -> HashMap String Int
buildMap ss = buildMap' ss HM.empty

buildMap' :: [String] -> HashMap String Int -> HashMap String Int
buildMap' (s:ss) m = buildMap' ss $ HM.insertWith (+) s 1 m
buildMap' [] m     = m
