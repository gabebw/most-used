module MostUsed.Types
    ( CommandName
    , Argument(..)
    , Command(..)
    , Shell(..)
    ) where

type CommandName = String

data Shell = Zsh | Bash

data Command = Command { commandName :: CommandName, arguments :: [Argument] }
            deriving (Show, Eq)

data Argument = DoubleQuoted String
              | SingleQuoted String
              | NotQuoted String
              | Backticks String
              | CommandSubstitution Command
              | ProcessSubstitution Command
              | Heredoc String
              deriving (Eq)

instance Show Argument where
    show (DoubleQuoted s)        = "\"" ++ s ++ "\""
    show (SingleQuoted s)        = "'" ++ s ++ "'"
    show (NotQuoted s)           = s
    show (Backticks s)           = "`" ++ s ++ "`"
    show (CommandSubstitution c) = "$(" ++ show c ++ ")"
    show (ProcessSubstitution c) = "<(" ++ show c ++ ")"
    show (Heredoc s)             = "<<<'" ++ s ++ "'"
