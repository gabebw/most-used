module MostUsed.Parser.Zsh
    ( items
    ) where

import Control.Monad (void)
import MostUsed.Parser.Common
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = do
    void $ some spaceChar
    void $ some digitChar -- history number
    void $ string "  "
    item `sepBy` pipe <* eof
