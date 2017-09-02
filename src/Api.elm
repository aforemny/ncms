module Api exposing ( apis )


import Json.Decode as Json
import Ncms.Backend.Github as Github
import Ncms.Backend.Ncms as Backend
import Ncms.Backend exposing (Rest,Prim(..))
import Api.User as User
import Api.Blog as Blog



-- API


apis : List (Rest msg)
apis =
  [ 
    { tipe =
          { name = "User"
          , idField = 
              { name = "username"
              , tipe = String
              }
          , fields =
              [
              { name = "email"
              , tipe = String
              }
              ]
          }
    , get = Github.get "user" identity Json.value
    , update = Github.update "user" identity Json.value (Github.idField "username")
    , delete = Github.delete "user" identity Json.value
    , create = Github.create "user" identity Json.value (Github.idField "username")
    , list = Github.list "user" identity Json.value
    }
  , 
    { tipe =
          { name = "Blog"
          , idField = 
              { name = "id"
              , tipe = String
              }
          , fields =
              [
              { name = "published"
              , tipe = String
              }
        ,              { name = "date"
              , tipe = String
              }
        ,              { name = "title"
              , tipe = String
              }
        ,              { name = "content"
              , tipe = String
              }
              ]
          }
    , get = Github.get "blog" identity Json.value
    , update = Github.update "blog" identity Json.value (Github.idField "id")
    , delete = Github.delete "blog" identity Json.value
    , create = Github.create "blog" identity Json.value (Github.idField "id")
    , list = Github.list "blog" identity Json.value
    }
  ] 
