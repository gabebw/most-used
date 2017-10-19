module MostUsed.Parser.Zsh
    ( items
    ) where

import           Control.Monad          (void)
import qualified MostUsed.Parser.Common as Common (items)
import           MostUsed.Types         (Command)
import           Text.Megaparsec        (digitChar, skipSome, spaceChar, string)
import           Text.Megaparsec.String (Parser)

items :: Parser [Command]
items = do
    skipSome spaceChar
    skipSome digitChar -- history number
    void $ string "  "
    Common.items
