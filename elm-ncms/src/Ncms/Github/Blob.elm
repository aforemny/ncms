module Ncms.Github.Blob exposing (..)

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)

import Ncms.Github.Internal exposing (..)


type alias Blob =
    { content : String
    , encoding : String
    , url : String
    , sha : String
    , size : Int
    }


defaultBlob : Blob
defaultBlob =
    { content = ""
    , encoding = ""
    , url = ""
    , sha = ""
    , size = -1
    }


getBlob : String -> String -> String -> String -> Task Error Blob
getBlob accessToken owner repo sha =
    let
        decode =
            succeed Blob
                |= (Json.at [ "content" ] (Json.map (String.split "\n" >> String.join "") Json.string))
                |= (Json.at [ "encoding" ] Json.string)
                |= (Json.at [ "url" ] Json.string)
                |= (Json.at [ "sha" ] Json.string)
                |= (Json.at [ "size" ] Json.int)
    in
    Http.toTask <|
    Http.request
      { method =
          "GET"
      , headers =
          [ Http.header "Authorization" ("token " ++ accessToken)
          ]
      , url =
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/blobs/" ++ sha
       , body =
           Http.emptyBody
       , expect =
           Http.expectJson decode
       , timeout=
           Nothing
       , withCredentials =
           False
       }


createBlob : String -> String -> String -> String -> Task Error { url : String, sha : String }
createBlob accessToken owner repo content =
    let
        decode =
            Json.map2 (\ url sha -> { url = url, sha = sha })
                (Json.at [ "url" ] Json.string)
                (Json.at [ "sha" ] Json.string)
        body =
            Encode.object
            [ ("content", Encode.string content )
            , ("encoding", Encode.string "utf-8")
            ]
    in
    Http.toTask <|
    Http.request
      { method =
          "POST"
      , headers =
          [ Http.header "Authorization" ("token " ++ accessToken)
          ]
      , url =
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/blobs"
       , body =
           Http.jsonBody body
       , expect =
           Http.expectJson decode
       , timeout=
           Nothing
       , withCredentials =
           False
       }
