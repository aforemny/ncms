module Api exposing ( apis )


import Ncms.Backend.Ncms as Backend
import Json.Decode as Json
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
        { get = Backend.get "user" identity Json.value
        , update = Backend.update "user" identity Json.value
        , delete = Backend.delete "user" identity Json.value
        , create = Backend.create "user" identity Json.value
        , list = Backend.list "/user" identity Json.value
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
        { get = Backend.get "blog" identity Json.value
        , update = Backend.update "blog" identity Json.value
        , delete = Backend.delete "blog" identity Json.value
        , create = Backend.create "blog" identity Json.value
        , list = Backend.list "/blog" identity Json.value
        }
    }
  ] 
