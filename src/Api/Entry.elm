module Api.Entry exposing
    ( 
      get
    , update
    , delete
    , create
    , list
    , Entry
    , defaultEntry
    , encodeEntry
    , entryDecoder
    )


import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ncms.Backend.Ncms as Backend


-- API


get : (Result Error Entry -> msg) -> String -> Cmd msg
get =
    Backend.get "entry" encodeEntry entryDecoder


update : (Result Error Entry -> msg) -> Entry -> Cmd msg
update =
    Backend.update "entry" encodeEntry entryDecoder .key


delete : (Result Error Entry -> msg) -> String -> Cmd msg
delete =
    Backend.delete "entry" encodeEntry entryDecoder


list : (Result Error (List Entry) -> msg) -> Cmd msg
list =
    Backend.list "/entry" encodeEntry entryDecoder


create : (Result Error Entry -> msg) -> Entry -> Cmd msg
create =
    Backend.create "entry" encodeEntry entryDecoder



-- TYPES


type alias Entry =
    { key : String
    , value : String
    }



defaultEntry : Entry
defaultEntry =
    { key = ""
    , value = ""
    }



-- DECODER


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map2 Entry
        ( Decode.at [ "key" ] Decode.string )
        ( Decode.at [ "value" ] Decode.string )




-- ENCODER


encodeEntry : Entry -> Decode.Value
encodeEntry value =
    [ ( "key", Encode.string value.key )
    , ( "value", Encode.string value.value )
    ]
    |> Encode.object



