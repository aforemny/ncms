module Core.Dict where

import qualified Data.Map as Data.Map

type Dict k a =
    Data.Map.Map k a


get =
    Data.Map.lookup


empty =
    Data.Map.empty


member =
    Data.Map.member


keys =
    Data.Map.keys


values =
    Data.Map.elems


map =
    Data.Map.map


indexedMap =
    Data.Map.mapWithKey


toList =
    Data.Map.toList


fromList =
    Data.Map.fromList
