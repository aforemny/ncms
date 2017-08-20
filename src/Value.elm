module Value exposing (..)

import Dict exposing (Dict)
import Json.Decode as Json
import Native.Json


type Value
    = Null
    | Bool Bool
    | String String
    | Number Float
    | List (List Value)
    | Object (Dict String Value)


expose : Json.Value -> Value
expose =
    Native.Json.expose
