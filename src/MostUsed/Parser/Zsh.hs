module MostUsed.Parser.Zsh
    ( items
    ) where

import           Control.Monad          (void)
import qualified MostUsed.Parser.Common as Common (items)
import           MostUsed.Types         (Command, Parser())
import           Text.Megaparsec.Char   (digitChar, spaceChar, string)
import           Text.Megaparsec        (skipSome)

items :: Parser [Command]
items = do
    skipSome spaceChar
    skipSome digitChar -- history number
    void $ string "  "
    Common.items
