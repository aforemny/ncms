module Api.Test exposing
    ( 
      get
    , update
    , delete
    , create
    , list
    , Test
    , defaultTest
    , encodeTest
    , testDecoder
    )


import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ncms.Backend.Github as Backend


-- API


get : (Result Error Test -> msg) -> String -> String -> String -> String -> Cmd msg
get =
    Backend.get "test" encodeTest testDecoder


update : (Result Error Test -> msg) -> String -> String -> String -> Test -> Cmd msg
update =
    Backend.create "test" encodeTest testDecoder .id


delete : (Result Error () -> msg) -> String -> String -> String -> String -> Cmd msg
delete =
    Backend.delete "test" encodeTest testDecoder


list : (Result Error (List Test) -> msg) -> String -> String -> String -> Cmd msg
list =
    Backend.list "test" encodeTest testDecoder


create : (Result Error Test -> msg) -> String -> String -> String -> Test -> Cmd msg
create =
    Backend.create "test" encodeTest testDecoder .id



-- TYPES


type alias Test =
    { id : String
    , bool : Bool
    , int : Int
    , float : Float
    }



defaultTest : Test
defaultTest =
    { id = ""
    , bool = False
    , int = -1
    , float = 0.0
    }



-- DECODER


testDecoder : Decoder Test
testDecoder =
    Decode.map4 Test
        ( Decode.at [ "id" ] Decode.string )
        ( Decode.at [ "bool" ] Decode.bool )
        ( Decode.at [ "int" ] Decode.int )
        ( Decode.at [ "float" ] Decode.float )




-- ENCODER


encodeTest : Test -> Decode.Value
encodeTest value =
    [ ( "id", Encode.string value.id )
    , ( "bool", Encode.bool value.bool )
    , ( "int", Encode.int value.int )
    , ( "float", Encode.float value.float )
    ]
    |> Encode.object



