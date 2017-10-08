module Core.List where

import Core.Basics
import qualified Data.Maybe
import qualified Prelude


type List a =
    [a]


length =
    Prelude.length


singleton x =
  [x]


map =
  Prelude.map


dropLeft =
    Prelude.drop


dropRight n =
    reverse . Prelude.drop n . reverse


foldl =
    Prelude.foldl -- TODO


foldr =
    Prelude.foldr -- TODO


filter =
    Prelude.filter


concat =
    Prelude.concat


filterMap pred =
    Data.Maybe.catMaybes . map pred


reverse =
    Prelude.reverse

(++) =
    (Prelude.++)


infixr 5 ++
