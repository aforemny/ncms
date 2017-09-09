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
import Material.Card as Card
import Material.Checkbox as Checkbox
import Material.List as Lists
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Textfield as Textfield
import Material.Typography as Typography
import Navigation
import Ncms.Backend as Backend exposing (Rest, Tipe)
import Task exposing (Task)
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
       , create : Value -> Task Http.Error ()
       , delete : String -> Task Http.Error ()
       , get : String -> Task Http.Error Value
       , list : Task Http.Error (List Value)
       , update : Value -> Task Http.Error ()
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
            ( model, attempt error (\ _ -> lift DeleteOk) (delete id) )

        DeleteOk ->
            ( model, attempt error (lift << List) list )


attempt fail cont =
    Task.attempt
        ( \ result ->
            case result of
                Err e -> fail e
                Ok x -> cont x
        )


init
    : (Msg msg -> msg)
    -> { tipe : Backend.Tipe
       , error : Http.Error -> msg
       , create : Value -> Task Http.Error ()
       , delete : String -> Task Http.Error ()
       , get : String -> Task Http.Error Value
       , list : Task Http.Error (List Value)
       , update : Value -> Task Http.Error ()
       }
    -> ( Model, Cmd msg )
init lift { error, list } =
    ( defaultModel, attempt error (lift << List) list )


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
               |> List.map Value.expose
               |> List.map (\ value ->
                      let
                          id =
                              case value of
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
                      fieldsRow (Just id) [] (fromValue fields value)
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
          |> List.map (\ { name, tipe } ->
                case value of
                    Value.Object kvs ->
                        let
                           f tipe value =
                              case ( tipe, value ) of
                                 ( Backend.String, Just (Value.String string)) ->
                                     string
                                 ( Backend.String, _) ->
                                     ""
                                 ( Backend.Bool, Just (Value.Bool bool)) ->
                                     toString bool
                                 ( Backend.Bool, _ ) ->
                                     "False"
                                 ( Backend.Int, Just (Value.Number number) ) ->
                                     toString number
                                 ( Backend.Int, _ ) ->
                                     "0"
                                 ( Backend.Float, Just (Value.Number number) ) -> 
                                     toString number
                                 ( Backend.Float, _ ) ->
                                     "0"
                                 ( Backend.Maybe _, Just Value.Null ) ->
                                     "–"
                                 ( Backend.Maybe tipe_, Just _ ) ->
                                     f tipe_ value
                                 ( Backend.Maybe _, _ ) ->
                                     "–"
                                 ( Backend.List tipe_, Just (Value.List values) ) ->
                                     values
                                     |> List.map (f tipe_ << Just)
                                     |> String.join ", "
                                 ( Backend.List tipe_, _ ) ->
                                     "[]"
                        in
                        f tipe (Dict.get name kvs)

                    _ ->
                        toString value
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
