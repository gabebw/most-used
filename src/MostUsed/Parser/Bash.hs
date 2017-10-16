module MostUsed.Parser.Bash
    ( items
    ) where

import qualified MostUsed.Parser.Common as Common (items)
import           MostUsed.Types         (Command)
import           Text.Megaparsec.String (Parser)

items :: Parser [Command]
items = Common.items
