module Pages.Listing exposing
    ( Model
    , defaultModel
    , Msg
    , update
    , init
    , view
    )

import Api
import Dict exposing (Dict)
import Html.Attributes as Html
import Html exposing (Html, text)
import Http
import Json.Decode exposing (Value)
import Material
import Material.Button as Button
import Material.List as Lists
import Material.Card as Card
import Material.Checkbox as Checkbox
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Textfield as Textfield
import Material.Typography as Typography
import Navigation
import Ncms.Backend as Backend exposing (Rest, Tipe)
import Value

import Page exposing ( ApiId(..), DataId(..) )


type alias Model =
    { mdl : Material.Model
    , values : List Value
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , values = []
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | Delete DataId
    | DeleteOk
    | List (List Value)


update
    : (Msg msg -> msg)
    -> { tipe : Backend.Tipe
       , error : Http.Error -> msg
       , create : (Result Http.Error Value -> msg) -> Value -> Cmd msg
       , delete : (Result Http.Error () -> msg) -> String -> Cmd msg
       , get : (Result Http.Error Value -> msg) -> String -> Cmd msg
       , list : (Result Http.Error (List Value) -> msg) -> Cmd msg
       , update : (Result Http.Error Value -> msg) -> Value -> Cmd msg
       }
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift { tipe, delete, list, error } msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model

        List values ->
            ( { model | values = values }, Cmd.none )

        (Delete (DataId id)) ->
            ( model, delete (handle error (\_ -> lift DeleteOk)) id )

        DeleteOk ->
            ( model, list (handle error (lift << List)) )


init
    : (Msg msg -> msg)
    -> { tipe : Backend.Tipe
       , error : Http.Error -> msg
       , create : (Result Http.Error Value -> msg) -> Value -> Cmd msg
       , delete : (Result Http.Error () -> msg) -> String -> Cmd msg
       , get : (Result Http.Error Value -> msg) -> String -> Cmd msg
       , list : (Result Http.Error (List Value) -> msg) -> Cmd msg
       , update : (Result Http.Error Value -> msg) -> Value -> Cmd msg
       }
    -> ( Model, Cmd msg )
init lift { error, list } =
    ( defaultModel, list (handle error (lift << List)) )


handle error msg result =
    case result of
        Ok x -> msg x
        Err e -> error e


view
    : (Msg msg -> msg)
    -> { navigate : Page.Page -> msg
       }
    -> Tipe
    -> Model
    -> Html msg
view lift { navigate } tipe model =
  let
      rowStyle =
          [ cs "row"
          , css "display" "flex"
          , css "flex-flow" "row"
          , css "flex" "1 1 auto"
          ]

      listingType { name, idField, fields } =
           Html.div []
           [ Card.view
             [ css "width" "1200px"
             , css "max-width" "100%"
             ]
             [ Card.primary []
               [
                 Card.title [ Card.large ] [ text (name ++ " listing") ]
               ]

             , Card.supportingText []
               [ listingFields ( idField :: fields )
               ]

             , Card.actions []
               [ Button.render (Mdl >> lift) [0,1,2,3] model.mdl
                 [ Options.onClick (navigate (Page.New (ApiId name)))
                 , Button.accent
                 ]
                 [ text "New"
                 ]
               ]
             ]
           ]

      listingFields fields =
           Lists.ul []
           ( List.concat
             [
               [ fieldsRow Nothing
                 [ css "color" "#ccc"
                 ] <|
                 List.map .name fields
               , Lists.divider [] []
               ]

             , model.values
               |> List.map (\ value ->
                      let
                          id =
                              case Value.expose value of
                                  Value.Object obj ->
                                      Dict.get tipe.idField.name obj
                                      |> Maybe.map (\ v ->
                                            case v of
                                                Value.String str -> str
                                                _ -> ""
                                         )
                                      |> Maybe.withDefault ""
                                  _ ->
                                      ""
                      in
                      fieldsRow (Just id) [] (fromValue fields (Value.expose value))
                  )
             ]
           )

      fieldsRow id options strs =
           let
               columnWidth =
                   toString (100 / toFloat (1 + List.length strs)) ++ "%"
           in
           Lists.li options
           [ Lists.text rowStyle <|
             List.concat
             [
               strs
               |> List.map (\ str ->
                      styled Html.div
                      [ css "width" columnWidth
                      ]
                      [ text str
                      ]
                 )
             , [ styled Html.div
                  [ css "display" "flex"
                  , css "flex-flow" "row"
                  , css "width" columnWidth
                  ]
                  [ styled Html.i
                    [ case id of
                          Just id ->
                              Options.onClick <|
                                  navigate (Page.Edit (ApiId tipe.name) (DataId id))
                          Nothing ->
                              Options.nop
                    , cs "material-icons"
                    ]
                    [ text "edit"
                    ]
                  , styled Html.i
                    [ case id of
                          Just id ->
                              Options.onClick (lift (Delete (DataId id)))
                          Nothing ->
                              Options.nop
                    , cs "material-icons"
                    ]
                    [ text "delete"
                    ]
                  ]
               ]
             ]
           ]

      fromValue fields value =
          fields
          |> List.map (\ { name } ->
                case value of
                    Value.Object kvs ->
                        Dict.get name kvs
                        |> Maybe.map (\ v ->
                               case v of
                                  Value.Null ->
                                      "null"
                                  Value.Bool bool ->
                                      toString bool
                                  Value.String str ->
                                      str
                                  Value.Number num ->
                                      toString num
                                  _ ->
                                      toString v
                           )
                        |> Maybe.withDefault ""

                    _ ->
                        ""
             )
  in
  Html.div
  [ Html.style
    [ ("font-size", "16px")
    , ("padding", "32px")
    ]
  ]
  [ listingType tipe
  ]
