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
              { name = "string"
              , tipe = String
              }
          , fields =
              [
              { name = "bool"
              , tipe = Bool
              }
        ,              { name = "int"
              , tipe = Int
              }
        ,              { name = "float"
              , tipe = Float
              }
        ,              { name = "maybeString"
              , tipe = Maybe (String)
              }
        ,              { name = "maybeBool"
              , tipe = Maybe (Bool)
              }
        ,              { name = "maybeInt"
              , tipe = Maybe (Int)
              }
        ,              { name = "maybeFloat"
              , tipe = Maybe (Float)
              }
              ]
          }
    , get = Backend.get "user" identity Json.value
    , update = Backend.update "user" identity Json.value (Backend.idField "string")
    , delete = Backend.delete "user" identity Json.value
    , create = Backend.create "user" identity Json.value (Backend.idField "string")
    , list = Backend.list "user" identity Json.value
    }
  ] 
