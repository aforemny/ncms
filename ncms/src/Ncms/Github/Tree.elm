module Ncms.Github.Tree exposing
    (
      Tree
    , defaultTree
    , File
    , defaultFile
    , getTree
    , getTreeRecursively
    , createTree
    )

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)

import Ncms.Github.Internal exposing (..)


type alias Tree a =
    { sha : String
    , url : String
    , tree : List a
    , truncated : Bool
    }


defaultTree : Tree a
defaultTree =
    { sha = ""
    , url = ""
    , tree = []
    , truncated = False
    }


type alias File =
    { path : String
    , mode : String
    , type_ : String
    , size : Maybe Int
    , sha : String
    , url : String
    }


defaultFile : File
defaultFile =
    { path = ""
    , mode = ""
    , type_ = ""
    , size = Nothing
    , sha = ""
    , url = ""
    }

getTree : String -> String -> String -> String -> Task Error (Tree File)
getTree =
    getTree_ False


getTreeRecursively : String -> String -> String -> String -> Task Error (Tree File)
getTreeRecursively =
    getTree_ True


getTree_ recursive accessToken owner repo sha =
    let
        decode =
            succeed Tree
                |= (Json.at [ "sha" ] Json.string)
                |= (Json.at [ "url" ] Json.string)
                |= (Json.at [ "tree" ] (Json.list decodeFile))
                |= (Json.at [ "truncated" ] Json.bool)

        decodeFile =
            succeed File
                |= (Json.at [ "path" ] Json.string)
                |= (Json.at [ "mode" ] Json.string)
                |= (Json.at [ "type" ] Json.string)
                |= ( Json.oneOf
                     [ Json.map Just (Json.at [ "size" ] Json.int)
                     , Json.succeed Nothing
                     ]
                   )
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
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/trees/" ++ sha
          |> if recursive then
                 flip (++) "?recursive=1"
             else
                 identity
       , body =
           Http.emptyBody
       , expect =
           Http.expectJson decodeTree
       , timeout=
           Nothing
       , withCredentials =
           False
       }


createTree
  : String
  -> String
  -> String
  -> { baseTree : Maybe String
     , tree : List
         { path : String
         , mode : String
         , type_ : String
         , sha : String
         }
     }
  -> Task Error (Tree File)
createTree accessToken owner repo { tree, baseTree } =
    let
        body =
            Encode.object
            ( List.filterMap identity
              [ Just ("tree", Encode.list (List.map encodeTree tree))
              , case baseTree of
                    Nothing ->
                        Nothing
                    Just baseTree ->
                        Just ("base_tree", Encode.string baseTree)
              ]
            )

        encodeTree { path, mode, type_, sha } =
            Encode.object
            [ ("path", Encode.string path)
            , ("mode", Encode.string mode)
            , ("type", Encode.string type_)
            , ("sha", Encode.string sha)
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
          "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/git/trees"
       , body =
           Http.jsonBody body
       , expect =
           Http.expectJson decodeTree
       , timeout=
           Nothing
       , withCredentials =
           False
       }


decodeTree =
    succeed Tree
        |= (Json.at [ "sha" ] Json.string)
        |= (Json.at [ "url" ] Json.string)
        |= (Json.at [ "tree" ] (Json.list decodeFile))
        |= (Json.at [ "truncated" ] Json.bool)


decodeFile =
    succeed File
        |= (Json.at [ "path" ] Json.string)
        |= (Json.at [ "mode" ] Json.string)
        |= (Json.at [ "type" ] Json.string)
        |= ( Json.oneOf
             [ Json.map Just (Json.at [ "size" ] Json.int)
             , Json.succeed Nothing
             ]
           )
        |= (Json.at [ "sha" ] Json.string)
        |= (Json.at [ "url" ] Json.string)
