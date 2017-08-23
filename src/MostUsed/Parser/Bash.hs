module MostUsed.Parser.Bash
    ( items
    ) where

import MostUsed.Parser.Common
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = separatedItems <* eof
