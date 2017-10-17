{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import           MostUsed
import           MostUsed.CLI.Display   (failedParses, successfulParses)
import           MostUsed.CLI.Parser    (Options (..), parseCLI)
import qualified MostUsed.Parser.Bash   as B
import qualified MostUsed.Parser.Zsh    as Z
import           MostUsed.Types         (Command, Shell (Bash, Zsh))
import           Text.Megaparsec.String (Parser)

main :: IO ()
main = do
    Options{..} <- parseCLI
    stdinContents <- getContents
    let display = if oDebug
        then failedParses
        else successfulParses oIncludeFirstArgument
    putStrLn $ display (parser oShell) stdinContents

parser :: Shell -> Parser [Command]
parser Bash = B.items
parser Zsh  = Z.items
