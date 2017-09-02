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
import Value


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
                _ ->
                    toString v
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


type alias Endpoint =
    String


type alias Rest msg =
    { tipe : Tipe
    , create :
          (Result Error Value -> msg)
          -> String
          -> String
          -> String
          -> Value
          -> Cmd msg
    , delete :
          (Result Error () -> msg)
          -> String
          -> String
          -> String
          -> String
          -> Cmd msg
    , get :
          (Result Error Value -> msg)
          -> String
          -> String
          -> String
          -> String
          -> Cmd msg
    , list :
          (Result Error (List Value) -> msg)
          -> String
          -> String
          -> String
          -> Cmd msg
    , update :
          (Result Error Value -> msg)
          -> String
          -> String
          -> String
          -> Value
          -> Cmd msg
    }


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
