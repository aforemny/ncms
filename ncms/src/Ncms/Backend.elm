module Ncms.Backend exposing
    ( Rest
    , Tipe
    , tipe
    , Field
    , field
    , Prim(..)

    , lookup
    , toValue
    , toInputs
    )

import Dict exposing (Dict)
import Http exposing (Error)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Value


type alias Rest a =
    { tipe : Tipe
    , create :
         Value
          -> Task Error ()
    , delete :
          String
          -> Task Error ()
    , get :
          String
          -> Task Error a
    , list :
          Task Error (List a)
    , update :
          Value
          -> Task Error ()
    }


type alias Tipe =
    { name : String
    , idField : Field
    , fields : List Field
    }


toInputs : Tipe -> Value -> Dict String String
toInputs { name, idField, fields } value =
    let
        toString v =
            case v of
                Value.String string ->
                    string
                Value.Bool bool ->
                    if bool then
                        "True"
                    else
                        "False"
                Value.Null ->
                    ""
                Value.Number num ->
                    Basics.toString num
                Value.List xs ->
                    "[]"
                Value.Object _ ->
                    "{}"
    in
    case Value.expose value of
        Value.Object obj ->
            obj
            |> Dict.map (\_ -> toString)
        _ ->
            Dict.empty


toValue : Tipe -> Dict String String -> Value
toValue { idField, fields } inputs =
    ( idField :: fields )
    |> List.map (\ { name, tipe } ->
           ( name
           , let
                 input =
                     Dict.get name inputs
             in
             case tipe of
                 String ->
                     case input of
                         Just string ->
                             Encode.string string
                         Nothing ->
                             Encode.string ""
                 Bool ->
                     case input of
                         Just string ->
                             Encode.bool (string == "True")
                         Nothing ->
                             Encode.bool False
                 Int ->
                     case input of
                         Just string ->
                             Encode.int (Result.withDefault 0 (String.toInt string))
                         Nothing ->
                             Encode.int 0
                 Float ->
                     case input of
                         Just string ->
                             Encode.float (Result.withDefault 0 (String.toFloat string))
                         Nothing ->
                             Encode.float 0
           )
       )
    |> Encode.object


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


type alias Endpoint =
    String


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
