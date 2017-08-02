module MostUsed.Parser.Bash
    ( items
    ) where

import MostUsed.Parser.Common
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Item]
items = item `sepBy` pipe <* eof
