module MostUsed.Types
    ( Command(..)
    , Argument(..)
    , Item(..)
    ) where

type Command = String

data Item = Item { command :: Command, arguments :: [Argument] }
            deriving (Show, Eq)

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
