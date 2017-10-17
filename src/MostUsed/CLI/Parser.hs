module MostUsed.CLI.Parser
    ( parseCLI
    )
    where

import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           MostUsed.Types
import           MostUsed.Version    (version)
import           Options.Applicative

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
    info (helper <*> opts <* versionOption) $ header h <> progDesc d <> footer f

parseOptions :: Parser Options
parseOptions = Options
    <$> parseIncludeFirstArgument
    <*> parseDebug
    <*> parseShell

versionOption :: Parser (a -> a)
versionOption = infoOption ("most-used version " ++ version) $
    long "version"
    <> short 'v'
    <> help "Display the version"

parseIncludeFirstArgument :: Parser [CommandName]
parseIncludeFirstArgument = many $ strOption $
    long "include-first-argument"
    <> metavar "command_name"
    <> help "Count this command with its first argument (can be specified more than once)"

parseShell :: Parser Shell
parseShell = fmap (fromMaybe Zsh) $ optional $ shell <$> o
    where
        o = strOption $
            long "shell"
            <> metavar "[bash | zsh]"
            <> help "Which type of shell history to parse (defaults to zsh)"

shell :: String -> Shell
shell "bash" = Bash
shell "zsh"  = Zsh
shell _      = Zsh

parseDebug :: Parser Bool
parseDebug = switch $
    long "debug"
    <> help "Print only lines that couldn't be parsed"
