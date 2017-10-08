module Core.String where

import Core.List (List)
import Prelude ((.))
import qualified Data.Text
import qualified Prelude

type String =
    Data.Text.Text


pack =
    Data.Text.pack


unpack =
    Data.Text.unpack


append =
    Data.Text.append


concat =
    Data.Text.concat


uncons =
    Data.Text.uncons


cons =
    Data.Text.cons


fromChar =
    pack . (:[])


writeFile fn =
    Prelude.writeFile fn . unpack


readFile fn =
    Prelude.fmap pack (Prelude.readFile fn)


join :: Data.Text.Text -> List Data.Text.Text -> Data.Text.Text
join =
    Data.Text.intercalate


split :: Data.Text.Text -> Data.Text.Text -> List Data.Text.Text
split =
    Data.Text.splitOn


(++) =
    append


infixr 5 ++
