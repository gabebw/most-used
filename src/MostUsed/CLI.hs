module MostUsed.CLI
    ( parseCLI
    , Options(..)
    )
    where

import MostUsed
import Data.Monoid ((<>))
import Options.Applicative

data Options = Options
    { oIncludeFirstArgument :: [Command]
    }

parseCLI :: IO Options
parseCLI =
    execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader      = "Most Used: Find your most-used CLI commands"
    pDescription = "Most used parses your commands and summarizes the \
                   \most-used ones"
    pFooter      = ""

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

parseOptions :: Parser Options
parseOptions = Options <$> parseIncludeFirstArgument

parseIncludeFirstArgument :: Parser [String]
parseIncludeFirstArgument = many $ strOption $
    long "include-first-argument"
    <> help "Count this command with its first argument (can be specified more than once)"
