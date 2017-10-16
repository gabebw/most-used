module MostUsed.Parser.Zsh
    ( items
    ) where

import Control.Monad (void)
import qualified MostUsed.Parser.Common as Common (items)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = do
    void $ some spaceChar
    void $ some digitChar -- history number
    void $ string "  "
    Common.items
