module Core.Maybe
   (
     Maybe(..)
   , withDefault
   , map
   , andThen
   , Core.Maybe.join
   )
   where

import Core.Basics
import Data.Maybe (Maybe(..))
import qualified Control.Monad
import qualified Data.Maybe


map :: (a -> b) -> Maybe a -> Maybe b
map =
    fmap


andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen f x = do
    v <- x
    f v


withDefault =
    flip Data.Maybe.maybe identity


join :: Maybe (Maybe a) -> Maybe a
join =
    Control.Monad.join
