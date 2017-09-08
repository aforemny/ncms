module Pages.Edit exposing
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
import Json.Encode exposing (Value)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Checkbox as Checkbox
import Material.Options as Options exposing (styled, cs, css, when)
import Material.Textfield as Textfield
import Material.Typography as Typography
import Navigation
import Ncms.Backend as Backend exposing (Rest, Tipe)
import Page exposing ( ApiId(..), DataId(..) )
import Task exposing (Task)
import Value


type alias Model =
    { mdl : Material.Model
    , inputs : Dict String String
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , inputs = Dict.empty
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | Cancel
    | Save
    | SaveOk
    | Input String String
    | Get Value


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
update lift { tipe, create, error } msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model

        Cancel ->
            ( model, Navigation.back 1 )

        Save ->
            ( model
            , attempt error (\_ -> lift SaveOk) <|
              create ( Backend.toValue tipe model.inputs )
            )

        SaveOk ->
            ( model, Navigation.back 1 )

        Input name value ->
            ( { model | inputs = Dict.insert name value model.inputs }, Cmd.none )

        (Get value) ->
            ( { model
                | inputs = Backend.toInputs tipe value
              }
            ,
              Cmd.none
            )


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
    -> Maybe DataId
    -> ( Model, Cmd msg )
init lift { error, get } dataId =
    case dataId of
        Nothing ->
            ( defaultModel, Cmd.none )
        Just (DataId id) ->
            ( defaultModel, attempt error (lift << Get) (get id) )


view : Bool -> (Msg msg -> msg) -> Tipe -> Model -> Html msg
view isCreate lift { name, idField, fields } model =
  Html.div
  [ Html.style
    [ ("font-size", "16px")
    , ("padding", "32px")
    ]
  ]
  [
    styled Html.h1 [ Typography.title ] [ text name ]

  , Card.view []
    [ Card.primary []
      [
        styled Html.div []
        ( ( idField :: fields )
          |> List.concat << List.indexedMap (\ i field ->
              [ Html.label []
                [ text field.name
                ]
              , case field.tipe of
                    Backend.Bool ->
                        styled Html.div
                        [ css "display" "block"
                        , css "margin-bottom" "32px"
                        ]
                        [
                          styled Html.div [ cs "mdc-form-field" ]
                          [ let
                              value =
                                  Dict.get field.name model.inputs
                                  |> Maybe.withDefault ""
                            in
                            Checkbox.render (Mdl >> lift) [1,0,i] model.mdl
                            [
                              Options.onClick <| lift << Input field.name <|
                              if value == "True" then "False" else "True"

                            , if value == "True" then
                                  Checkbox.checked
                              else
                                  Options.nop
                            ]
                            []

                          , Html.label
                            [
                            ]
                            [ text field.name
                            ]
                          ]
                        ]
                    Backend.String ->
                        Textfield.render (Mdl >> lift) [1,0,i] model.mdl
                            [ Options.onInput (Input field.name >> lift)
                            , Textfield.value
                                ( Dict.get field.name model.inputs
                                  |> Maybe.withDefault ""
                                )
                            , when ( ( field.name == idField.name ) && not isCreate) <|
                              Textfield.disabled
                            , Textfield.fullWidth
                            , css "margin-bottom" "32px"
                            ]
                            []
                    Backend.Int ->
                        Textfield.render (Mdl >> lift) [1,0,i] model.mdl
                            [ Options.onInput (Input field.name >> lift)
                            , Textfield.value
                                ( Dict.get field.name model.inputs
                                  |> Maybe.withDefault ""
                                )
                            , when ( ( field.name == idField.name ) && not isCreate) <|
                              Textfield.disabled
                            , Textfield.fullWidth
                            , css "margin-bottom" "32px"
                            ]
                            []
                    Backend.Float ->
                        Textfield.render (Mdl >> lift) [1,0,i] model.mdl
                            [ Options.onInput (Input field.name >> lift)
                            , Textfield.value
                                ( Dict.get field.name model.inputs
                                  |> Maybe.withDefault ""
                                )
                            , when ( ( field.name == idField.name ) && not isCreate) <|
                              Textfield.disabled
                            , Textfield.fullWidth
                            , css "margin-bottom" "32px"
                            ]
                            []
              ]
            )
        )
      ]

    , Card.actions
      [ css "display" "flex"
      , css "flex-flow" "row-reverse"
      ]
      [ Button.render (Mdl >> lift) [1,1,0] model.mdl
        [ Options.onClick (lift Save)
        , Button.primary
        ]
        [ text "Save"
        ]
      , Button.render (Mdl >> lift) [1,1,0] model.mdl
        [ Options.onClick (lift Cancel)
        ]
        [ text "Cancel"
        ]
      ]
    ]
  ]
