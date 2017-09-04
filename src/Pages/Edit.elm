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
import Value
import Page exposing ( ApiId(..), DataId(..) )


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
       , create : (Result Http.Error Value -> msg) -> Value -> Cmd msg
       , delete : (Result Http.Error () -> msg) -> String -> Cmd msg
       , get : (Result Http.Error Value -> msg) -> String -> Cmd msg
       , list : (Result Http.Error (List Value) -> msg) -> Cmd msg
       , update : (Result Http.Error Value -> msg) -> Value -> Cmd msg
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
            , create (handle error (\_ -> lift SaveOk)) <|
                  Backend.toValue tipe model.inputs
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
    -> Maybe DataId
    -> ( Model, Cmd msg )
init lift { error, get } dataId =
    case dataId of
        Nothing ->
            ( defaultModel, Cmd.none )
        Just (DataId id) ->
            ( defaultModel, get (handle error (lift << Get)) id )


handle error msg result =
    case result of
        Ok x -> msg x
        Err e -> error e


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
                            [ text "True" ]
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
