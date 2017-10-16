{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import           MostUsed
import           MostUsed.CLI           (Options (..), parseCLI)
import           MostUsed.CLI.Display   (displayFailures, displaySuccesses)
import qualified MostUsed.Parser.Bash   as B
import qualified MostUsed.Parser.Zsh    as Z
import           MostUsed.Types         (Command, Shell (Bash, Zsh))
import           Text.Megaparsec.String (Parser)

main :: IO ()
main = do
    Options{..} <- parseCLI
    stdinContents <- getContents
    let f = if oDebug
            then displayFailures
            else displaySuccesses oIncludeFirstArgument
    f (parser oShell) stdinContents

parser :: Shell -> Parser [Command]
parser Bash = B.items
parser Zsh  = Z.items
