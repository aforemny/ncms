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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ncms.Backend.Github as Backend


-- API


get : (Result Error User -> msg) -> String -> String -> String -> String -> Cmd msg
get =
    Backend.get "user" encodeUser userDecoder


update : (Result Error User -> msg) -> String -> String -> String -> User -> Cmd msg
update =
    Backend.create "user" encodeUser userDecoder .username


delete : (Result Error () -> msg) -> String -> String -> String -> String -> Cmd msg
delete =
    Backend.delete "user" encodeUser userDecoder


list : (Result Error (List User) -> msg) -> String -> String -> String -> Cmd msg
list =
    Backend.list "user" encodeUser userDecoder


create : (Result Error User -> msg) -> String -> String -> String -> User -> Cmd msg
create =
    Backend.create "user" encodeUser userDecoder .username



-- TYPES


type alias User =
    { username : String
    , email : String
    }



defaultUser : User
defaultUser =
    { username = ""
    , email = ""
    }



-- DECODER


userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        ( Decode.at [ "username" ] Decode.string )
        ( Decode.at [ "email" ] Decode.string )




-- ENCODER


encodeUser : User -> Decode.Value
encodeUser value =
    [ ( "username", Encode.string value.username )
    , ( "email", Encode.string value.email )
    ]
    |> Encode.object



