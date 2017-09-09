module Api.User exposing
    ( 
      get
    , update
    , delete
    , create
    , list
    , User
    , defaultUser
    , encodeUser
    , userDecoder
    )


import Http exposing (Error)
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ncms.Backend.Ncms as Backend


-- API


get : String -> Task Error User
get =
    Backend.get "user" encodeUser userDecoder


update : User -> Task Error ()
update =
    Backend.create "user" encodeUser userDecoder .string


delete : String -> Task Error ()
delete =
    Backend.delete "user" encodeUser userDecoder


list : Task Error (List User)
list =
    Backend.list "user" encodeUser userDecoder


create : User -> Task Error ()
create =
    Backend.create "user" encodeUser userDecoder .string



-- TYPES


type alias User =
    { string : String
    , bool : Bool
    , int : Int
    , float : Float
    , maybeString : Maybe (String)
    , listString : List (String)
    }



defaultUser : User
defaultUser =
    { string = ""
    , bool = False
    , int = -1
    , float = 0.0
    , maybeString = Nothing
    , listString = []
    }



-- DECODER


userDecoder : Decoder User
userDecoder =
    Decode.map6 User
        ( Decode.at [ "string" ] Decode.string )
        ( Decode.at [ "bool" ] Decode.bool )
        ( Decode.at [ "int" ] Decode.int )
        ( Decode.at [ "float" ] Decode.float )
        ( Decode.at [ "maybeString" ] (Decode.maybe Decode.string) )
        ( Decode.at [ "listString" ] (Decode.list Decode.string) )




-- ENCODER


encodeUser : User -> Decode.Value
encodeUser value =
    [ ( "string", Encode.string value.string )
    , ( "bool", Encode.bool value.bool )
    , ( "int", Encode.int value.int )
    , ( "float", Encode.float value.float )
    , ( "maybeString", (Maybe.withDefault Encode.null << Maybe.map (Encode.string)) value.maybeString )
    , ( "listString", Encode.list << List.map Encode.string<| value.listString )
    ]
    |> Encode.object



