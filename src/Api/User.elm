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
    Backend.create "user" encodeUser userDecoder .username


delete : String -> Task Error ()
delete =
    Backend.delete "user" encodeUser userDecoder


list : Task Error (List User)
list =
    Backend.list "user" encodeUser userDecoder


create : User -> Task Error ()
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



