module Ncms.Github.Reference exposing (..)

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)

import Ncms.Github.Internal exposing (..)


type alias Reference =
    { ref : String
    , url : String
    , object : Object
    }


defaultReference : Reference
defaultReference =
    { ref = ""
    , url = ""
    , object = defaultObject
    }


type alias Object =
    { type_ : String
    , sha : String
    , url : String
    }


defaultObject =
    { type_ = ""
    , sha = ""
    , url = ""
    }


getReference : String -> String -> String -> String -> Task Error Reference
getReference accessToken owner repo ref =
    let
        decode =
            succeed Reference
                |= (Json.at [ "ref" ] Json.string)
                |= (Json.at [ "url" ] Json.string)
                |= (Json.at [ "object" ] decodeObject)

        decodeObject =
            succeed Object
                |= (Json.at [ "type" ] Json.string)
                |= (Json.at [ "sha" ] Json.string)
                |= (Json.at [ "url" ] Json.string)
    in
    Http.toTask <|
    Http.request
      { method =
          "GET"
      , headers =
          [ Http.header "Authorization" ("token " ++ accessToken)
          ]
      , url =
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/refs/" ++ ref
       , body =
           Http.emptyBody
       , expect =
           Http.expectJson decode
       , timeout=
           Nothing
       , withCredentials =
           False
       }


updateReference : String -> String -> String -> String -> { sha : String, force : Bool } -> Task Error Reference
updateReference accessToken owner repo ref { sha, force } =
    let
        body =
            Encode.object
            [ ("sha", Encode.string sha)
            , ("force", Encode.bool force)
            ]
    in
    Http.toTask <|
    Http.request
      { method =
          "PATCH"
      , headers =
          [ Http.header "Authorization" ("token " ++ accessToken)
          ]
      , url =
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/refs/" ++ ref
       , body =
           Http.jsonBody body
       , expect =
           Http.expectJson decodeReference
       , timeout=
           Nothing
       , withCredentials =
           False
       }


decodeReference =
    succeed Reference
        |= (Json.at [ "ref" ] Json.string)
        |= (Json.at [ "url" ] Json.string)
        |= (Json.at [ "object" ] decodeObject)

decodeObject =
    succeed Object
        |= (Json.at [ "type" ] Json.string)
        |= (Json.at [ "sha" ] Json.string)
        |= (Json.at [ "url" ] Json.string)
