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


toInputs : Tipe -> Value -> Dict String Value.Value
toInputs { name, idField, fields } value =
    case Value.expose value of
        Value.Object obj ->
            obj
        _ ->
            Dict.empty


toValue : Tipe -> Dict String Value.Value -> Value
toValue { idField, fields } inputs =
    let
        f tipe input =
           case (tipe, input) of
               (String, Just (Value.String string)) ->
                   Encode.string string

               (String, _) ->
                   Encode.string ""

               (Bool, Just (Value.Bool bool)) ->
                   Encode.bool bool

               (Bool, _) ->
                   Encode.bool False

               (Int, Just (Value.Number number)) ->
                   if number == toFloat (floor number) then
                       Encode.int (floor number)
                   else
                       Encode.int 0

               (Int, _) ->
                   Encode.int 0

               (Float, Just (Value.Number number)) ->
                   Encode.float number

               (Float, _) ->
                   Encode.float 0

               (Maybe tipe_, Just Value.Null) ->
                   Encode.null

               (Maybe tipe_, Just _) ->
                   f tipe_ input

               (Maybe tipe_, _) ->
                   Encode.null
    in
    ( idField :: fields )
    |> List.map (\ { name, tipe } ->
           ( name, f tipe (Dict.get name inputs) )
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
    | Maybe Prim


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
