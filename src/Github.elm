module Github exposing (..)

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Task exposing (Task)

type alias User =
    { login : String
    , id : Int
    , avatarUrl : String
    , gravatarId : String
    , url : String
    , htmlUrl : String
    , followersUrl : String
    , followingUrl : String
    , gistsUrl : String
    , starredUrl : String
    , subscriptionsUrl : String
    , organizationsUrl : String
    , reposUrl : String
    , eventsUrl : String
    , receivedEventsUrl : String
    , type_ : String
    , siteAdmin : Bool
    , name : String
    , company : Maybe String
    , blog : String
    , location : String
    , email : Maybe String
    , hireable : Maybe Bool
    , bio : Maybe String
    , publicRepos : Int
    , publicGists : Int
    , followers : Int
    , following : Int
    , createdAt : String
    , updatedAt : String
    }


defaultUser : User
defaultUser =
    { login = ""
    , id = -1
    , avatarUrl = ""
    , gravatarId = ""
    , url = ""
    , htmlUrl = ""
    , followersUrl = ""
    , followingUrl = ""
    , gistsUrl = ""
    , starredUrl = ""
    , subscriptionsUrl = ""
    , organizationsUrl = ""
    , reposUrl = ""
    , eventsUrl = ""
    , receivedEventsUrl = ""
    , type_ = ""
    , siteAdmin = False
    , name = ""
    , company = Nothing
    , blog = ""
    , location = ""
    , email = Nothing
    , hireable = Nothing
    , bio = Nothing
    , publicRepos = -1
    , publicGists = -1
    , followers = -1
    , following = -1
    , createdAt = "2000-01-00T00:00:00Z"
    , updatedAt = "2000-01-00T00:00:00Z"
    }


user : String -> Task Error User
user accessToken =
    let
        decode : Decoder User
        decode =
            succeed User
                |= (Json.at [ "login" ] Json.string)
                |= (Json.at [ "id" ] Json.int)
                |= (Json.at [ "avatar_url" ] Json.string)
                |= (Json.at [ "gravatar_id" ] Json.string)
                |= (Json.at [ "url" ] Json.string)
                |= (Json.at [ "html_url" ] Json.string)
                |= (Json.at [ "followers_url" ] Json.string)
                |= (Json.at [ "following_url" ] Json.string)
                |= (Json.at [ "gists_url" ] Json.string)
                |= (Json.at [ "starred_url" ] Json.string)
                |= (Json.at [ "subscriptions_url" ] Json.string)
                |= (Json.at [ "organizations_url" ] Json.string)
                |= (Json.at [ "repos_url" ] Json.string)
                |= (Json.at [ "events_url" ] Json.string)
                |= (Json.at [ "received_events_url" ] Json.string)
                |= (Json.at [ "type" ] Json.string)
                |= (Json.at [ "site_admin" ] Json.bool)
                |= (Json.at [ "name" ] Json.string)
                |= (Json.at [ "company" ] (Json.maybe Json.string))
                |= (Json.at [ "blog" ] Json.string)
                |= (Json.at [ "location" ] Json.string)
                |= (Json.at [ "email" ] (Json.maybe Json.string))
                |= (Json.at [ "hireable" ] (Json.maybe Json.bool))
                |= (Json.at [ "bio" ] (Json.maybe Json.string))
                |= (Json.at [ "public_repos" ] Json.int)
                |= (Json.at [ "public_gists" ] Json.int)
                |= (Json.at [ "followers" ] Json.int)
                |= (Json.at [ "following" ] Json.int)
                |= (Json.at [ "created_at" ] Json.string)
                |= (Json.at [ "updated_at" ] Json.string)
    in
    Http.toTask <|
    Http.request
      { method =
          "GET"
      , headers =
          [ Http.header "Authorization" ("token " ++ accessToken)
          ]
      , url =
          "https://api.github.com/user"
       , body =
           Http.emptyBody
       , expect =
           Http.expectJson decode
       , timeout=
           Nothing
       , withCredentials =
           False
       }


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


tree : String -> Bool -> String -> String -> String -> Task Error (Tree File)
tree accessToken recursive owner repo sha =
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
           Http.expectJson decode
       , timeout=
           Nothing
       , withCredentials =
           False
       }


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


reference : String -> String -> String -> String -> Task Error Reference
reference accessToken owner repo ref =
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


blob : String -> String -> String -> String -> Task Error Blob
blob accessToken owner repo sha =
    let
        decode =
            succeed Blob
                |= (Json.at [ "content" ] Json.string)
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


succeed : a -> Decoder a
succeed =
    Json.succeed


(|=)  : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) mf mx =
    Json.andThen (\f -> Json.map f mx ) mf


infixl 0 |=
