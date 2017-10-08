module Core.Basics where

import Core.Bool
import Prelude (Maybe(Just, Nothing))
import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Trans
import qualified Data.Function
import qualified Data.Text as Text
import qualified Prelude
import qualified System.Directory as Directory
import System.IO.Unsafe


concat =
    Prelude.concat


type IO a =
    Prelude.IO a


(&) x f =
    (Data.Function.&) x f


infixl 1 &


void =
  const ()


const =
  Prelude.const


fmap =
  Prelude.fmap


print =
  Prelude.print


($) =
  (Prelude.$)


infixr 0 $


(.) =
  (Prelude..)


infixr 9 .


error =
  Prelude.error


pure =
  Prelude.return


head xs =
  case xs of
      [] -> Nothing
      ( x : _ ) -> Just x


(/=) =
  (Prelude./=)


infix 4 /=


(==) =
    (Prelude.==)

infix 4 ==


toLowercase str =
    case Text.uncons str of
        Just (firstLetter, rest) ->
          Text.toLower (Text.pack ([firstLetter])) `Text.append` rest
        Nothing ->
            str


debug s x =
    unsafePerformIO (Prelude.putStrLn (s Prelude.++ ": " Prelude.++ Prelude.show x))
        `Prelude.seq` x


identity =
    Prelude.id


flip =
    Prelude.flip


type IOError =
    Prelude.IOError


(-) =
    (Prelude.-)

infixl 6 -


(+) =
    (Prelude.+)

infixl 6 +


isPrefixOf pre str =
    pre == Prelude.take (Prelude.length pre) str


isSuffixOf suf str =
    suf == Prelude.drop (Prelude.length str - Prelude.length suf) str


mkdir =
    Directory.createDirectoryIfMissing True


(++) =
    (Prelude.++)


infixr 5 ++


(<*) =
    (Control.Applicative.<*)


infixl 4 <*


(*>) =
    (Control.Applicative.*>)


infixl 4 *>


(<*>) =
    (Control.Applicative.<*>)


infixl 4 <*>


(<|>) =
    (Control.Applicative.<|>)


infixl 3 <|>


(<$>) =
    (Control.Applicative.<$>)


infixl 4 <$>


liftIO =
    Control.Monad.Trans.liftIO


(>>=) =
    (Prelude.>>=)


infixl 1 >>=


(=<<) =
    (Prelude.=<<)


infixr 1 =<<


return =
    Prelude.return


mapM =
    Prelude.mapM


mapM_ =
    Prelude.mapM_


forM =
    Control.Monad.forM


forM_ =
    Control.Monad.forM_


join =
    Control.Monad.join


sequence =
    Control.Monad.sequence


unless =
    Control.Monad.unless


when =
    Control.Monad.when


catch =
    Control.Exception.catch
