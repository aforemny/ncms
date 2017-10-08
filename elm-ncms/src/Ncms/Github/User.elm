module Ncms.Github.User exposing (..)

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)

import Ncms.Github.Internal exposing (..)


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


getUser : String -> Task Error User
getUser accessToken =
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
