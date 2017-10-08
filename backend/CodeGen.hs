module CodeGen
  ( makeElmUserType
  , makeElmCmsType
  )
  where

import Core
import Parser (Module(..), ApiDecl(..), TypeDecl(..), TypeRep(..))
import qualified Core.Dict as Dict
import qualified Core.List as List
import qualified Core.Maybe as Maybe
import qualified Core.String as String
import qualified Core.Tuple as Tuple


makeElmUserType mod_@(Module (ApiDecl kind type_ idField) types _) = do
  let
      moduleName =
          "Api." `String.append` type_

      fp =
          "src/Api/" `String.append` type_ `String.append` ".elm"
          & String.unpack
  String.writeFile fp (CodeGen.elmApi moduleName mod_)


makeElmCmsType modules = do
  String.writeFile "src/Api.elm" (CodeGen.elmApis modules)


elmApis modules =
    let
        (+) =
            String.append

        showTypeRep typeRep =
            case typeRep of
                TString -> "String"
                TBool -> "Bool"
                TInt -> "Int"
                TFloat -> "Float"
                TMaybe typeRep -> "Maybe (" + showTypeRep typeRep + ")"
                TList typeRep -> "List (" + showTypeRep typeRep + ")"
    in
    String.join "\n"
    [ "module Api exposing ( apis )"
    , "\n"
    , "import Http exposing (Error)"
    , "import Json.Decode as Json exposing (Value)"
    , "import Ncms.Backend exposing (Rest,Prim(..))"
    , "import Ncms.Backend.Ncms as Backend"
    , "import Task exposing (Task)"
    , modules
      & List.map (\ (Module (ApiDecl kind type_ idField) types _) ->
          "import Api." + type_ + " as " + type_
        )
      & String.join "\n"
    , "\n"
    , "-- API"
    , "\n"
    , modules
      & List.map (\ (Module (ApiDecl kind type_ idField) types _) ->
          let
              endpoint =
                  toLowercase type_

              mainType =
                Dict.values types
                & List.filter (\ (TypeDecl typeName fields) ->
                      typeName == type_
                  )
                & head
                & Maybe.withDefault (error "no main type")

              otherTypes =
                  Dict.values types
                  & List.filter (\ (TypeDecl typeName fields) ->
                        typeName /= type_
                    )

              makeType (TypeDecl typeName fields) =
                  let
                      ( idField : otherFields ) =
                          fields

                      makeField ( fieldName, typeRep ) =
                          [ "name = \"" + fieldName + "\""
                          , "tipe = " + makePrim typeRep
                          ]
                          & String.join "\n              , "
                          & String.append "              { "
                          & flip String.append "\n              }"

                      makePrim typeRep =
                          case typeRep of
                              TString ->
                                  "String"
                              TBool ->
                                  "Bool"
                              TInt ->
                                  "Int"
                              TFloat ->
                                  "Float"
                              TMaybe typeRep' ->
                                  "Maybe (" + makePrim typeRep' + ")"
                              TList typeRep' ->
                                  "List (" + makePrim typeRep' + ")"
                  in
                  [ "name = \"" + typeName + "\""
                  , "idField = \n" + makeField idField
                  , otherFields
                    & List.map makeField
                    & String.join "\n        ,"
                    & String.append "fields =\n              [\n"
                    & flip String.append "\n              ]"
                  ]
                  & String.join "\n          , "
                  & String.append "          { "
                  & flip String.append "\n          }"
        in
        [
          "tipe =\n" + makeType mainType

        , ( case kind of
                "ncms" ->
                    [ "get = Backend.get \"" + endpoint + "\" identity Json.value"
                    , "update = Backend.update \"" + endpoint + "\" identity Json.value (Backend.idField \"" + idField + "\")"
                    , "delete = Backend.delete \"" + endpoint + "\" identity Json.value"
                    , "create = Backend.create \"" + endpoint + "\" identity Json.value (Backend.idField \"" + idField + "\")"
                    , "list = Backend.list \"" + endpoint + "\" identity Json.value"
                    ]
                "github" ->
                    [ "get = Github.get \"" + endpoint + "\" identity Json.value"
                    , "update = Github.update \"" + endpoint + "\" identity Json.value (Github.idField \"" + idField + "\")"
                    , "delete = Github.delete \"" + endpoint + "\" identity Json.value"
                    , "create = Github.create \"" + endpoint + "\" identity Json.value (Github.idField \"" + idField + "\")"
                    , "list = Github.list \"" + endpoint + "\" identity Json.value"
                    ]
                _ ->
                    error "unsupported type"
          )
          & String.join "\n    , "
        ]
        & String.join "\n    , "
        & String.append "\n    { "
        & flip String.append "\n    }"
      )
    & String.join "\n  , "
    & String.append "apis =\n  [ "
    & String.append "apis : List (Rest Value)\n"
    & flip String.append "\n  ] "
  ]


elmApi modName (Module (ApiDecl kind type_ idField) types _) =
  let
      endpoint =
          toLowercase type_

      exposing =
          List.concat
          [ [ "get"
            , "update"
            , "delete"
            , "create"
            , "list"
            ]
          , Dict.keys types
          , Dict.keys types
            & List.map (\ typeName -> "default" + typeName)
          , Dict.keys types
            & List.map (\ typeName -> "encode" + typeName)
          , Dict.keys types
            & List.map (\ typeName -> toLowercase typeName + "Decoder")
          ]

      (+) =
          String.append

      indexTypeRep =
          types
          & Dict.get type_
          & Maybe.map
            ( \ type_ ->
              case type_ of
                  ( TypeDecl _ fields ) ->
                      fields
                      & List.filter ( (==) idField . Tuple.first )
                      & ( \xs ->
                            case xs of
                                [ ( _, typeRep ) ] -> typeRep
                                _ -> error "indexTypeRep"
                        )
            )
          & Maybe.withDefault (error "indexTypeRep")

      get =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "get : " + showTypeRep indexTypeRep + " -> Task Error " + type_
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "get : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      delete =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "delete : " + showTypeRep indexTypeRep + " -> Task Error ()"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "delete : (Result Error () -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      update =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "update : " + type_ + " -> Task Error ()"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  String.join "\n"
                  [ "update : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      create =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "create : " + type_ + " -> Task Error ()"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  String.join "\n"
                  [ "create : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      list =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "list : Task Error (List " + type_ + ")"
                  , "list ="
                  , "    Backend.list \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "list : (Result Error (List " + type_ + ") -> msg) -> String -> String -> String -> Cmd msg"
                  , "list ="
                  , "    Backend.list \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      api =
          [ get
          , update
          , delete
          , list
          , create
          ]
          & String.join "\n\n"
          & flip String.append "\n\n"

      encoders =
          let

              encodeTypeRep typeRep =
                  case typeRep of
                      TString -> "Encode.string"
                      TBool -> "Encode.bool"
                      TInt -> "Encode.int"
                      TFloat -> "Encode.float"
                      TMaybe typeRep ->
                          "(Maybe.withDefault Encode.null << Maybe.map ("
                          + encodeTypeRep typeRep + "))"
                      TList typeRep ->
                          "Encode.list << List.map " + encodeTypeRep typeRep + "<|"

              decodeTypeRep (TypeDecl typeName fields)  =
                  String.join "\n"
                  [ "encode" + typeName + " : " + typeName + " -> Decode.Value"
                  , "encode" + typeName + " value ="
                  , fields
                    & List.map (\ ( fieldName, typeRep ) ->
                        "( \"" + fieldName + "\", "
                        + encodeTypeRep typeRep  + " value." + fieldName + " )"
                      )
                    & String.join "\n    , "
                    & String.append "    [ "
                    & flip String.append "\n    ]"
                  , "    |> Encode.object"
                  ]
          in
          Dict.values types
          & List.map decodeTypeRep
          & String.join "\n\n"
          & flip String.append "\n\n"

      decoders =
          let
              typeRepDecoder typeRep =
                  case typeRep of
                      TString -> "Decode.string"
                      TBool -> "Decode.bool"
                      TInt -> "Decode.int"
                      TFloat -> "Decode.float"
                      TMaybe typeRep -> "(Decode.maybe " + typeRepDecoder typeRep + ")"
                      TList typeRep -> "(Decode.list " + typeRepDecoder typeRep + ")"


              decodeTypeRep (TypeDecl typeName fields)  =
                  String.join "\n"
                  [ toLowercase typeName + "Decoder : Decoder " + typeName
                  , toLowercase typeName + "Decoder ="
                  , fields
                    & List.map (\ ( fieldName, typeRep ) ->
                        "        ( Decode.at [ \"" + fieldName + "\" ] "
                        + typeRepDecoder typeRep  + " )"
                      )
                    & String.join "\n"
                    & String.append (
                          if List.length fields == 1 then
                              "    Decode.map " + typeName + "\n"
                          else
                              "    Decode.map" + String.pack (show (List.length fields)) + " " + typeName + "\n"
                      )
                  ]
          in
          Dict.values types
          & List.map decodeTypeRep
          & String.join "\n\n"
          & flip String.append "\n\n"

      showTypeRep typeRep =
          case typeRep of
              TString -> "String"
              TBool -> "Bool"
              TInt -> "Int"
              TFloat -> "Float"
              TMaybe typeRep -> "Maybe (" + showTypeRep typeRep + ")"
              TList typeRep -> "List (" + showTypeRep typeRep + ")"

      typeDefs =
          Dict.values types
          & List.map ( \ (TypeDecl typeName fields) ->
                String.join "\n"
                [ "type alias "  + typeName + " ="
                , fields
                  & List.map (\ (fieldName, typeRep) ->
                        fieldName + " : " + showTypeRep typeRep
                    )
                  & String.join "\n    , "
                  & String.append "    { "
                  & flip String.append "\n    }"
                ]
            )
          & String.join "\n\n"

      typeRepDefault typeRep =
          case typeRep of
              TString -> "\"\""
              TBool -> "False"
              TInt -> "-1"
              TFloat -> "0.0"
              TMaybe typeRep -> "Nothing"
              TList typeRep -> "[]"

      defaultTypes =
          Dict.values types
          & List.map ( \ (TypeDecl typeName fields) ->
                String.join "\n"
                [ "default" + typeName + " : " + typeName
                , "default" + typeName + " ="
                , fields
                  & List.map (\ (fieldName, typeRep) ->
                        fieldName + " = " + typeRepDefault typeRep
                    )
                  & String.join "\n    , "
                  & String.append "    { "
                  & flip String.append "\n    }"
                ]
            )
          & String.join "\n\n"
  in
  String.join "\n"
  [ "module " + modName + " exposing"
  , "    ( "
  ,
    exposing
    & String.join "\n    , "
    & String.append "      "

  , "    )"
  , "\n"
  , "import Http exposing (Error)"
  , "import Task exposing (Task)"
  , "import Json.Decode as Decode exposing (Decoder)"
  , "import Json.Encode as Encode"
  , case kind of
      "ncms" ->
        "import Ncms.Backend.Ncms as Backend"
      "github" ->
        "import Ncms.Backend.Github as Backend"
      _ ->
        error "unsupported kind"
  , "\n"

  , "-- API"
  , "\n"
  , api

  , "-- TYPES"
  , "\n"
  , typeDefs
  , "\n"
  , defaultTypes
  , "\n"

  , "-- DECODER"
  , "\n"
  , decoders

  , "-- ENCODER"
  , "\n"
  , encoders
  ]
