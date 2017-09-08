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
    , inputs : Dict String Value.Value
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
    | Input String Value.Value
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

  , let
        renderField : Int -> { name : String, tipe : Backend.Prim } -> Maybe Value.Value -> List (Html msg)
        renderField i field value =
            [ Html.label []
              [ text field.name
              ]
            , case field.tipe of
                  Backend.Bool ->
                      styled Html.div [ css "display" "block"
                      , css "margin-bottom" "32px"
                      ]
                      [
                        styled Html.div [ cs "mdc-form-field" ]
                        [ 
                          Checkbox.render (Mdl >> lift) [1,0,i] model.mdl
                          [
                            Options.onClick <| lift << Input field.name <|
                            case value of
                                Just (Value.Bool bool) ->
                                    Value.Bool (not bool)
                                _ ->
                                    Value.Bool True

                          , if value == Just (Value.Bool True) then
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
                          [ Options.onInput (lift << Input field.name << Value.String)
                          , Textfield.value
                              ( case value of
                                    Just (Value.String string) ->
                                        string
                                    _ ->
                                        ""
                              )
                          , when ( ( field.name == idField.name ) && not isCreate) <|
                            Textfield.disabled
                          , Textfield.fullWidth
                          , css "margin-bottom" "32px"
                          ]
                          []
                  Backend.Int ->
                      Textfield.render (Mdl >> lift) [1,0,i] model.mdl
                          [ Options.onInput (lift << Input field.name << Value.Number << toFloat << Result.withDefault 0 << String.toInt)
                          , Textfield.value
                              ( case value of
                                    Just (Value.Number number) ->
                                        if number == toFloat (floor number) then
                                            toString (floor number)
                                        else
                                            "0"
                                    _ ->
                                        "0"
                              )
                          , when ( ( field.name == idField.name ) && not isCreate) <|
                            Textfield.disabled
                          , Textfield.fullWidth
                          , css "margin-bottom" "32px"
                          ]
                          []
                  Backend.Float ->
                      Textfield.render (Mdl >> lift) [1,0,i] model.mdl
                          [ Options.onInput (lift << Input field.name << Value.Number << Result.withDefault 0 << String.toFloat)
                          , Textfield.value
                              ( case value of
                                    Just (Value.Number number) ->
                                        toString number
                                    _ ->
                                        "0"
                              )
                          , when ( ( field.name == idField.name ) && not isCreate) <|
                            Textfield.disabled
                          , Textfield.fullWidth
                          , css "margin-bottom" "32px"
                          ]
                          []
                  Backend.Maybe tipe_ ->
                      let
                          defaultValue tipe =
                              case tipe of
                                  Backend.String ->
                                      Value.String ""
                                  Backend.Bool ->
                                      Value.Bool False
                                  Backend.Int ->
                                      Value.Number 0
                                  Backend.Float ->
                                      Value.Number 0
                                  Backend.Maybe tipe_ ->
                                      defaultValue tipe_
                      in
                      styled Html.div [ css "display" "block"
                      , css "margin-bottom" "32px"
                      ]
                      [
                        styled Html.div [ cs "mdc-form-field" ]
                        [ 
                          Checkbox.render (Mdl >> lift) [1,0,i] model.mdl
                          [
                            Options.onClick <| lift << Input field.name <|
                            case value of
                                Just Value.Null ->
                                    defaultValue tipe_
                                Just value_ ->
                                    Value.Null
                                _ ->
                                    defaultValue tipe_
                          , case value of
                                Just Value.Null ->
                                    Options.nop
                                Just _ ->
                                    Checkbox.checked
                                _ ->
                                    Options.nop
                          ]
                          []

                        , Html.label []
                          ( case value of
                                Just Value.Null ->
                                    [ text field.name
                                    ]
                                Just value_ ->
                                    renderField i
                                        { name = field.name, tipe = tipe_ }
                                        (Just value_)
                                _ ->
                                    [ text field.name
                                    ]
                          )
                        ]
                      ]
            ]
    in
    Card.view []
    [ Card.primary []
      [
        styled Html.div []
        ( ( idField :: fields )
          |> List.concat << List.indexedMap (\ i field ->
                 let
                      value =
                          Dict.get field.name model.inputs
                 in
                 renderField i field value
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
