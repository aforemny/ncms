module Api exposing ( apis )


import Http exposing (Error)
import Json.Decode as Json exposing (Value)
import Ncms.Backend exposing (Rest,Prim(..))
import Ncms.Backend.Ncms as Backend
import Task exposing (Task)
import Api.User as User



-- API


apis : List (Rest Value)
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
    , get = Backend.get "user" identity Json.value
    , update = Backend.update "user" identity Json.value (Backend.idField "username")
    , delete = Backend.delete "user" identity Json.value
    , create = Backend.create "user" identity Json.value (Backend.idField "username")
    , list = Backend.list "user" identity Json.value
    }
  ] 
