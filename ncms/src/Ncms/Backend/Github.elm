module Ncms.Backend.Github exposing
    ( get
    , update
    , delete
    , list
    , create
    , idField
    )

import Base64
import Http exposing (Error)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Ncms.Github as Github
import Regex
import Sha256 exposing (sha256)
import Task


get : String -> (a -> Value) -> Decoder a -> (Result Error a -> msg) -> String -> String -> String -> String -> Cmd msg
get endpoint encode decoder cont accessToken owner repo id =
    Github.getReference accessToken owner repo "heads/gh-pages"
    |> Task.andThen (\ reference ->
          Github.getTreeRecursively accessToken owner repo reference.object.sha
       )
    |> Task.andThen (\ { tree } ->
          tree
          |> List.filter (\ file ->
                Regex.contains (Regex.regex ("^" ++ endpoint ++ "/" ++ id ++ "\\.json$")) file.path
             )
          |> List.head
          |> Maybe.map (\ file ->
                Github.getBlob accessToken owner repo file.sha
                |> Task.map (\ blob ->
                      case Base64.decode blob.content of
                          Ok content ->
                              case Json.decodeString decoder content of
                                  Ok obj ->
                                      obj
                                  _ ->
                                      Debug.crash "no decode"
                          Err _ ->
                              Debug.crash "non-base64 content"
                   )
             )
          |> Maybe.withDefault (Task.fail Http.NetworkError) -- TODO
       )
    |> Task.attempt cont


update : String -> (a -> Value) -> Decoder a -> (a -> String) -> (Result Error a -> msg) -> String -> String -> String -> a -> Cmd msg
update =
    create


delete : String -> (a -> Value) -> Decoder a -> (Result Error () -> msg) -> String -> String -> String -> String -> Cmd msg
delete endpoint encode decoder cont accessToken owner repo id =
    let
        fn =
            id ++ ".json"
    in
    Github.getReference accessToken owner repo "heads/gh-pages"
    |> Task.andThen (\ reference ->
           Github.getCommit accessToken owner repo reference.object.sha
       )
    |> Task.andThen (\ commit ->
           Task.map ((,) commit) <|
           Github.getTree accessToken owner repo commit.tree.sha
       )
    |> Task.andThen (\ ( commit, tree ) ->
           let
              subtree =
                 tree.tree
                 |> List.filter (\ file ->
                        file.path == endpoint
                    )
                 |> List.head
           in
           case subtree of
              Just subtree ->
                 Github.getTree accessToken owner repo subtree.sha
                 |> Task.andThen  (\ subtree ->
                       Github.createTree accessToken owner repo
                       { baseTree = Nothing
                       , tree =
                           subtree.tree
                           |> List.filterMap (\ { path, mode, type_, sha } ->
                                  { path = path
                                  , mode = mode
                                  , type_ = type_
                                  , sha = sha
                                  }
                                  |> if path == fn then
                                         always Nothing
                                     else
                                         Just
                              )
                       }
                    )
                 |> Task.map ((,) tree)
              Nothing ->
                  Debug.crash "update: vanished"
       )
    |> Task.andThen (\ (tree, subtree) ->
        Github.createTree accessToken owner repo
        { baseTree = Nothing
        , tree =
            tree.tree
            |> List.filterMap (\ { path, mode, type_, sha } ->
                   { path = path
                   , mode = mode
                   , type_ = type_
                   , sha = sha
                   }
                   |> if path == endpoint then
                          always Nothing
                      else
                          Just
               )
            |> (::)
               ( { path = endpoint
                 , mode = "040000"
                 , type_ = "tree"
                 , sha = subtree.sha
                 }
               )
        }
       )
    |> Task.andThen (\ tree ->
         Github.createCommit accessToken owner repo
         { message =
             "Delete " ++ endpoint ++ "/" ++ fn
         , tree =
             tree.sha
         , parents =
             []
         }
       )
    |> Task.andThen (\ commit ->
          Github.updateReference accessToken owner repo "heads/gh-pages"
            { sha = commit.sha
            , force = True
            }
       )
    |> Task.andThen (\ _ -> Task.succeed ())
    |> Task.attempt cont


list : String -> (a -> Value) -> Decoder a -> (Result Error (List a) -> msg) -> String -> String -> String -> Cmd msg
list endpoint encode decoder cont accessToken owner repo =
    Github.getReference accessToken owner repo "heads/gh-pages"
    |> Task.andThen (\ reference ->
          Github.getTreeRecursively accessToken owner repo reference.object.sha
       )
    |> Task.andThen (\ { tree } ->
          tree
          |> List.filter (\ file ->
                Regex.contains (Regex.regex ("^" ++ endpoint ++ "/.*\\.json$")) file.path
             )
          |> List.map (\ file ->
                Github.getBlob accessToken owner repo file.sha
             )
          |> Task.sequence
          |> Task.map (\ blobs ->
                blobs
                |> List.map (\ blob ->
                      case Base64.decode blob.content of
                          Ok content ->
                              case Json.decodeString decoder content of
                                  Ok obj ->
                                      obj
                                  _ ->
                                      Debug.crash "no decode"
                          Err _ ->
                              Debug.crash "non-base64 content"
                   )
             )
       )
    |> Task.attempt cont


create : String -> (a -> Value) -> Decoder a -> (a -> String) -> (Result Error a -> msg) -> String -> String -> String -> a -> Cmd msg
create endpoint encode decoder toId cont accessToken owner repo obj =
    let
        id =
            toId obj

        fn =
            endpoint ++ "/" ++ id ++ ".json"

        content =
            Encode.encode 4 (encode obj)
    in
    Github.getReference accessToken owner repo "heads/gh-pages"
    |> Task.andThen (\ reference ->
           Github.getCommit accessToken owner repo reference.object.sha
       )
    |> Task.andThen (\ commit ->
         Task.map ((,) commit) <|
         Github.createBlob accessToken owner repo
             content
       )
    |> Task.andThen (\ (commit, blob) ->
         Task.map ((,) commit) <|
         Github.createTree accessToken owner repo
         { baseTree = Just commit.tree.sha
         , tree =
             [ { path = fn
               , mode = "100644"
               , type_ = "blob"
               , sha = blob.sha
               }
             ]
         }
       )
    |> Task.andThen (\ (commit, tree) ->
          Task.map ((,) commit) <|
          Github.getTreeRecursively accessToken owner repo tree.sha
       )
    |> Task.andThen (\ (commit, tree) ->
         Task.map ((,) tree) <|
         Github.createCommit accessToken owner repo
         { message =
             "Create " ++ fn
         , tree =
             tree.sha
         , parents =
             [ commit.sha ]
         }
       )
    |> Task.andThen (\ (tree, commit) ->
          Task.map ((,,) tree commit) <|
          Github.updateReference accessToken owner repo "heads/gh-pages"
            { sha = commit.sha
            , force = True
            }
       )
    |> Task.andThen (\ ( { tree }, commit, reference ) ->
        let
            file =
                tree
                |> List.filter ((==) fn << .path)
                |> List.head
        in
        case file of
             Just file ->
                 Github.getBlob accessToken owner repo file.sha
                 |> Task.map (\ blob ->
                       case Base64.decode blob.content of
                           Ok content ->
                               case Json.decodeString decoder content of
                                   Ok obj ->
                                       obj
                                   _ ->
                                       Debug.crash "no decode"
                           Err _ ->
                               Debug.crash "non-base64 content"
                    )
             _ -> 
                Task.fail Http.NetworkError -- TODO
       )
    |> Task.attempt cont


request : String -> String -> String -> Http.Body -> Decoder a -> Http.Request a
request accessToken method url body decoder =
    Http.request
    { method =
        method
    , headers =
        [ Http.header "Accept" "application/json"
        , Http.header "Authorization" ("token " ++ accessToken)
        ]
    , url =
        url
    , body =
        body
    , expect =
        Http.expectJson decoder
    , timeout =
        Nothing
    , withCredentials =
        False
    }


idField : String -> Value -> String
idField field obj =
    case Json.decodeValue (Json.at [ field ] Json.string) obj of
        Ok x ->
            x
        Err e ->
            Debug.crash ("Ncms.Backend.Github.idField: " ++ e)
