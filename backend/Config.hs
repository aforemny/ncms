module Config where

import Core


data Config =
    Config
    { stateDirectory :: FilePath
    , dataDirectory :: FilePath
    , imageDirectory :: FilePath
    , sourceDirectory :: FilePath
    , logDirectory :: FilePath
    , siteDirectory :: FilePath
    }


defaultConfig :: Config
defaultConfig =
    Config
    { stateDirectory =
        "./_ncms"
    , dataDirectory =
        "./_ncms/data"
    , imageDirectory =
        "./_ncms/images"
    , sourceDirectory =
        "./_ncms/source"
    , logDirectory =
        "./_ncms/log"
    , siteDirectory =
        "./_ncms/site"
    }
