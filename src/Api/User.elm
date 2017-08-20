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
import Ncms.Backend.Ncms as Backend


-- API


get : (Result Error User -> msg) -> String -> Cmd msg
get =
    Backend.get "user" encodeUser userDecoder


update : (Result Error User -> msg) -> User -> Cmd msg
update =
    Backend.update "user" encodeUser userDecoder .username


delete : (Result Error User -> msg) -> String -> Cmd msg
delete =
    Backend.delete "user" encodeUser userDecoder


list : (Result Error (List User) -> msg) -> Cmd msg
list =
    Backend.list "/user" encodeUser userDecoder


create : (Result Error User -> msg) -> User -> Cmd msg
create =
    Backend.create "user" encodeUser userDecoder



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



