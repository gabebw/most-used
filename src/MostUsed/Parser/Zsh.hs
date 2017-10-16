module MostUsed.Parser.Zsh
    ( items
    ) where

import           Control.Monad          (void)
import qualified MostUsed.Parser.Common as Common (items)
import           MostUsed.Types         (Command)
import           Text.Megaparsec        (digitChar, some, spaceChar, string)
import           Text.Megaparsec.String (Parser)

items :: Parser [Command]
items = do
    void $ some spaceChar
    void $ some digitChar -- history number
    void $ string "  "
    Common.items
