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
import Json.Encode as Encode exposing (Value)
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
    , input : Input
    }


type Input
    = Lit String
    | List (List Input)
    | Maybe (Maybe Input)
    | Object (Dict String Input)


fromValue : Value.Value -> Input
fromValue value =
    case value of
        Value.Null ->
            Lit ""
        Value.Bool bool ->
            Lit (toString bool)
        Value.String str ->
            Lit str
        Value.Number float ->
            Lit (toString float)
        Value.List elements ->
            List (List.map fromValue elements)
        Value.Object kvs ->
            Object (Dict.map (\_ -> fromValue) kvs)

toValue : Tipe -> Input -> Value
toValue { idField, fields } input =
    let
        expectedInput =
            case input of
                Object kvs ->
                    kvs
                _ ->
                    Dict.empty

        cast prim input =
            case (prim, input) of
                ( Backend.String, Lit s ) ->
                    Encode.string s
                ( Backend.String, _ ) ->
                    Encode.string ""
                ( Backend.Bool, Lit "True" ) ->
                    Encode.bool True
                ( Backend.Bool, _ ) ->
                    Encode.bool False
                ( Backend.Int, Lit s ) ->
                    case String.toInt s of
                        Ok int ->
                            Encode.int int
                        Err _ ->
                            Encode.int 0
                ( Backend.Int, _ ) ->
                      Encode.int 0
                ( Backend.Float, Lit s ) ->
                      case String.toFloat s of
                          Ok float ->
                              Encode.float float
                          Err _ ->
                              Encode.float 0
                ( Backend.Float, _ ) ->
                      Encode.float 0
                ( Backend.Maybe prim_, Maybe (Just input_) ) ->
                      cast prim_ input_
                ( Backend.Maybe _, _ ) ->
                      Encode.null
                ( Backend.List prim_, List elements ) ->
                      Encode.list (List.map (cast prim_) elements)
                ( Backend.List _, _ ) ->
                      Encode.list []
    in
    Encode.object
    ( ( idField :: fields )
      |> List.map (\ field ->
             ( field.name
             ,
               cast field.tipe
                 ( Dict.get field.name expectedInput
                   |> Maybe.withDefault defaultInput
                 )
             )
         )
    )


defaultInput : Input
defaultInput =
    Lit ""


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , input = defaultInput
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | Cancel
    | Save
    | SaveOk
    | Input String Input
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
              create (toValue tipe model.input)
            )

        SaveOk ->
            ( model, Navigation.back 1 )

        Input fieldName newInput ->
            let
                splittedFieldNames =
                    let
                        result =
                            String.split "." fieldName
                    in
                    if result == [""] then
                        []
                    else
                        result
            in
            ( { model
                  | input =
                        updateInput (splittedFieldNames, newInput) model.input
              }
            ,
              Cmd.none
            )

        (Get value) ->
            ( { model | input = fromValue (Value.expose value) }, Cmd.none )


updateInput (fieldNames, newInput) input =
    case fieldNames of
        [] ->
            newInput
        ( fieldName :: otherFieldNames ) ->
            case String.toInt fieldName of
                Ok listIndex ->
                    let
                        expectedInput =
                            case input of
                                List elements ->
                                    let
                                        n =
                                            List.length elements
                                    in
                                    (++) elements
                                    ( List.repeat
                                          (min 0 (n-listIndex-1))
                                          defaultInput
                                    )
                                _ ->
                                    []

                        updatedInput =
                            updateInput (otherFieldNames, newInput)
                            ( expectedInput
                              |> List.indexedMap (\ i element ->
                                     if i == listIndex then
                                         Just element
                                     else Nothing
                                 )
                              |> List.filterMap identity
                              |> List.head
                              |> Maybe.withDefault defaultInput
                            )
                    in
                    expectedInput
                    |> List.indexedMap (\ i element ->
                           if i == listIndex then
                               updatedInput
                           else
                               element
                       )
                    |> List

                Err _ ->
                    let
                        expectedInput =
                            case input of
                                Object kvs ->
                                    kvs
                                _ ->
                                    Dict.empty

                        updatedInput =
                            updateInput (otherFieldNames, newInput)
                            ( Dict.get fieldName expectedInput
                              |> Maybe.withDefault defaultInput
                            )
                    in
                    Object (Dict.insert fieldName updatedInput expectedInput)


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


renderField
    : (Msg msg -> msg)
    -> List Int
    -> Material.Model
    -> Backend.Field
    -> { disabled : Bool
       , input : String -> Input -> Msg msg
       }
    -> Input
    -> Html msg
renderField lift idx mdl field options input =
    let
        renderBool =
            let
                expectedInput =
                    case input of
                        Lit s ->
                            s
                        _ ->
                            "False"
                isTrue =
                    expectedInput == "True"

                toggledInput =
                    if expectedInput == "True" then
                        Lit "False"
                    else
                        Lit "True"
            in
            styled Html.div
            [ css "display" "block"
            , css "margin-bottom" "32px"
            ]
            [ styled Html.div [ cs "mdc-form-field" ]
              [ 
                Checkbox.render (Mdl >> lift) idx mdl
                [
                  Options.onClick <| lift (options.input field.name toggledInput)

                , when isTrue <|
                  Checkbox.checked
                ]
                []

              , Html.label
                [
                ]
                [ text field.name
                ]
              ]
            ]

        renderString =
            let
                expectedInput =
                    case input of
                        Lit s ->
                            s
                        _ ->
                            ""
            in
            Textfield.render (Mdl >> lift) idx mdl
            [ Options.onInput (lift << options.input field.name << Lit)
            , Textfield.value expectedInput
            , Textfield.fullWidth
            , css "margin-bottom" "32px"
            ]
            []

        renderInt =
            let
                expectedInput =
                    case input of
                        Lit s ->
                            s
                        _ ->
                            "0"
            in
            Textfield.render (Mdl >> lift) idx mdl
            [ Options.onInput (lift << options.input field.name << Lit)
            , Textfield.value expectedInput
            , Textfield.fullWidth
            , css "margin-bottom" "32px"
            ]
            []

        renderFloat =
            let
                expectedInput =
                    case input of
                        Lit s ->
                            s
                        _ ->
                            "0"
            in
            Textfield.render (Mdl >> lift) idx mdl
                [ Options.onInput (lift << options.input field.name << Lit)
                , Textfield.value expectedInput
                , Textfield.fullWidth
                , css "margin-bottom" "32px"
                ]
                []

        renderMaybe tipe =
            let
                expectedInput =
                    case input of
                        Maybe input ->
                            input
                        _ ->
                            Nothing

                isNothing =
                    expectedInput == Nothing

                toggledInput =
                  if isNothing then
                      Maybe (Just defaultInput)
                  else
                      Maybe Nothing
            in
            styled Html.div
            [ css "display" "block"
            , css "margin-bottom" "32px"
            ]
            [
              styled Html.div [ cs "mdc-form-field" ]
              [ 
                Checkbox.render (Mdl >> lift) idx mdl
                [
                  Options.onClick <| lift << options.input field.name <|
                  toggledInput
                , when (not isNothing) Checkbox.checked
                ]
                []

              , Html.label []
                [ if isNothing then
                      text field.name
                  else
                      renderField lift idx mdl
                          { name = field.name, tipe = tipe }
                          { disabled = False
                          , input =
                              \ fieldName -> options.input fieldName << Maybe << Just
                          }
                          (Maybe.withDefault defaultInput expectedInput)
                ]
              ]
            ]

        renderList tipe =
            let
                expectedInput =
                    case input of
                        List elements ->
                            elements
                        _ ->
                            []
            in
            styled Html.div
            [ css "display" "block"
            , css "margin-bottom" "32px"
            ]
            ( List.concat
              [ expectedInput
                |> List.indexedMap (\ i ->
                       renderField lift (idx ++ [i]) mdl
                           { name =
                                 field.name
                                 |> String.split "."
                                 |> List.drop 1
                                 |> String.join "."
                           , tipe =
                                 tipe
                           }
                           { disabled = False
                           , input =
                               \ fieldName ->
                                   options.input (field.name ++ "." ++ toString i)
                           }
                  )

              , [ Button.render (Mdl >> lift) idx mdl
                  [ Options.onClick << lift << options.input field.name << List <|
                    expectedInput ++ [ defaultInput ]
                  ]
                  [ text "Add"
                  ]
                ]
              ]
            )
    in
    styled Html.div []
    [
      Html.label [] [ text field.name ]

    , case field.tipe of
          Backend.Bool ->
              renderBool

          Backend.String ->
              renderString

          Backend.Int ->
              renderInt

          Backend.Float ->
              renderFloat

          Backend.Maybe tipe_ ->
              renderMaybe tipe_

          Backend.List tipe_ ->
              renderList tipe_
    ]


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
          |> List.indexedMap (\ i field ->
                 renderField lift [i] model.mdl
                     field
                     { disabled = False
                     , input = Input
                     }
                     ( let
                           expectedInput =
                               case model.input of
                                   Object kvs ->
                                       kvs
                                   _ ->
                                       Dict.empty
                       in
                       Dict.get field.name expectedInput
                       |> Maybe.withDefault defaultInput
                     )
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
