module Ncms.Backend exposing
    ( Rest
    , Tipe
    , tipe
    , Field
    , field
    , Prim(..)

    , defaultValue
    , lookup
    )

import Dict exposing (Dict)
import Http exposing (Error)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Value


type alias Rest a =
    { tipe : Tipe
    , list : Task Error (List a)
    , get : String -> Task Error a
    , create : Value -> Task Error ()
    , update : Value -> Task Error ()
    , delete : String -> Task Error ()
    }


type alias Tipe =
    { name : String
    , idField : Field
    , fields : List Field
    }


tipe : String -> Field -> List Field -> Tipe
tipe name idField fields =
    { name = name
    , idField = idField
    , fields = fields
    }


type alias Field =
    { name : String
    , tipe : Prim
    }


field : String -> Prim -> Field
field name tipe =
    { name = name
    , tipe = tipe
    }


type Prim
    = String
    | Bool
    | Int
    | Float
    | Maybe Prim
    | List Prim


defaultValue : Prim -> Value.Value
defaultValue prim =
    case prim of
        String ->
            Value.String ""

        Bool ->
            Value.Bool False

        Int ->
            Value.Number 0

        Float ->
            Value.Number 0

        Maybe _ ->
            Value.Null

        List _ ->
            Value.List []


lookup : String -> List (Rest msg) -> Maybe (Rest msg)
lookup apiId apis =
    apis
    |> List.filterMap (\api ->
           if api.tipe.name == apiId then
               Just api
           else
              Nothing
       )
    |> List.head
