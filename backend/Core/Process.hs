module Core.Process where

import Core
import qualified Core.String as String
import qualified System.Process as Process


shell :: String -> List String -> String -> IO String
shell program args stdin = do
    let
        proc =
            Process.shell (String.unpack (String.join " " (program:args)))
    fmap String.pack (Process.readCreateProcess proc (String.unpack stdin))
