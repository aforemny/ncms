module Api exposing ( apis )


import Json.Decode as Json
import Ncms.Backend.Github as Github
import Ncms.Backend.Ncms as Backend
import Api.User as User
import Api.Blog as Blog



-- API


apis =
  [ 
    { type_ = "User"
    , idField = "username"
    , types = 
        [ { type_ = "User"
          , fields = 
            [ ( "username", "String" )
            , ( "email", "String" )
            ]
          }
        ]
    , api = 
        { get = Github.get "user" identity Json.value
        , update = Github.update "user" identity Json.value (Github.idField "username")
        , delete = Github.delete "user" identity Json.value
        , create = Github.create "user" identity Json.value (Github.idField "username")
        , list = Github.list "user" identity Json.value
        }
    }
  , 
    { type_ = "Blog"
    , idField = "id"
    , types = 
        [ { type_ = "Blog"
          , fields = 
            [ ( "id", "String" )
            , ( "published", "Bool" )
            , ( "date", "String" )
            , ( "title", "String" )
            , ( "content", "String" )
            ]
          }
        ]
    , api = 
        { get = Github.get "blog" identity Json.value
        , update = Github.update "blog" identity Json.value (Github.idField "id")
        , delete = Github.delete "blog" identity Json.value
        , create = Github.create "blog" identity Json.value (Github.idField "id")
        , list = Github.list "blog" identity Json.value
        }
    }
  ] 
