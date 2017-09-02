module Ncms.Github.Commit exposing (..)

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)

import Ncms.Github.Internal exposing (..)


type alias Commit =
    { sha : String
    , url : String
    , author : Author
    , committer : Author
    , message : String
    , tree : { sha : String, url : String }
    , parents : List { sha : String, url : String }
    }


defaultCommit =
    { sha = ""
    , url = ""
    , author = defaultAuthor
    , committer = defaultAuthor
    , message = ""
    , tree = { sha = "", url = "" }
    , parents = []
    }


type alias Author =
    { date : String
    , name : String
    , email : String
    }


defaultAuthor =
    { date = ""
    , name = ""
    , email = ""
    }


getCommit
  : String
  -> String
  -> String
  -> String
  -> Task Error Commit
getCommit accessToken owner repo sha =
    Http.toTask <|
    Http.request
    { method =
        "GET"
    , headers =
        [ Http.header "Authorization" ("token " ++ accessToken)
        ]
    , url =
        "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/commits/" ++ sha
     , body =
         Http.emptyBody
     , expect =
         Http.expectJson decodeCommit
     , timeout=
         Nothing
     , withCredentials =
         False
     }


createCommit
  : String
  -> String
  -> String
  -> { message : String
     , tree : String
     , parents : List String
     }
  -> Task Error Commit
createCommit accessToken owner repo { message, tree, parents } =
    let
        body =
            Encode.object
            [ (,) "message" (Encode.string message)
            , (,) "tree" (Encode.string tree)
            , (,) "parents" (Encode.list (List.map Encode.string parents))
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
        "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/commits"
     , body =
         Http.jsonBody body
     , expect =
         Http.expectJson decodeCommit
     , timeout=
         Nothing
     , withCredentials =
         False
     }

decodeCommit =
    succeed Commit
        |= Json.at [ "sha" ] Json.string
        |= Json.at [ "url" ] Json.string
        |= Json.at [ "author" ] decodeAuthor
        |= Json.at [ "committer" ] decodeAuthor
        |= Json.at [ "message" ] Json.string
        |= Json.at [ "tree" ] decodeTree
        |= Json.at [ "parents" ] (Json.list decodeTree)


decodeAuthor =
    succeed Author
        |= Json.at [ "date" ] Json.string
        |= Json.at [ "name" ] Json.string
        |= Json.at [ "email" ] Json.string

decodeTree =
    Json.map2
        ( \ sha url ->
            { sha = sha, url = url }
        )
        ( Json.at [ "sha" ] Json.string
        )
        ( Json.at [ "url" ] Json.string
        )
