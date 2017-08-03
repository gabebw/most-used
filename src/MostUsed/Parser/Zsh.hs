module MostUsed.Parser.Zsh
    ( items
    ) where

import MostUsed.Parser.Common
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = do
    some spaceChar
    some digitChar -- history number
    string "  "
    item `sepBy` pipe <* eof
