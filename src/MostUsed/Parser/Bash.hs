module MostUsed.Parser.Bash
    ( items
    ) where

import qualified MostUsed.Parser.Common as Common (items)
import MostUsed.Types
import Text.Megaparsec
import Text.Megaparsec.String

items :: Parser [Command]
items = Common.items
