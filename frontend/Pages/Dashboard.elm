module Pages.Dashboard exposing (..)

import Api
import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Checkbox as Checkbox
import Material.Drawer.Permanent as Drawer
import Material.Elevation as Elevation
import Material.List as Lists
import Material.Options as Options exposing (styled, css, cs, when)
import Material.Textfield as Textfield
import Material.Theme as Theme
import Material.Toolbar as Toolbar
import Material.Typography as Typography
import Navigation exposing (Location)
import Ncms.Backend as Backend
import Ncms.Github as Github
import Regex
import Task
import Task exposing (Task)


type alias Model =
    { mdl : Material.Model
    , commits : List Github.Commit
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , commits = []
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | Init (List Github.Commit)

update
    : (Msg msg -> msg)
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model
        Init commits ->
            ( { model | commits = commits }, Cmd.none )


handle error succeed result =
    case result of
        Err e -> error e
        Ok x -> succeed x

init
    : (Msg msg -> msg)
    -> {}
    -> ( Model, Cmd msg )
init lift _ =
    ( defaultModel, Cmd.none )


view lift _ model =
    Html.div []
    [
      styled Html.h1 [ Typography.title ] [ text "Dashboard" ]
    , Card.view
      [ css "max-width" "900px"
      ]
      [ Lists.ul []
        ( model.commits
          |> List.map (\ commit ->
                Lists.li
                [ css "display" "flex"
                , css "flex-flow" "row"
                ]
                [ styled Html.span
                  [ css "width" "80px"
                  ]
                  [ text (String.left 6 commit.sha)
                  ]
                , Html.span [] [ text " " ]
                , styled Html.span
                  [ css "width" "300px" ]
                  [ text commit.message ]
                , Html.span [] [ text " " ]
                , styled Html.span
                  [ css "width" "140px" ]
                  [ text (toString commit.author.date) ]
                ]
             )
        )
      ]

--    , Card.view
--      [ css "max-width" "600px"
--      ]
--      [
--        Card.primary []
--        [ Card.title [ Card.large ] [ text "Login" ]
--        ]
--
--      , Card.supportingText
--        [ css "display" "flex"
--        , css "flex-flow" "column"
--        ]
--        [
--          Html.p []
--          [ text "Obtain credentials by "
--          , Html.a
--            [ Html.href "https://github.com/settings/applications/new"
--            ]
--            [ text "registering an application" ]
--          , text " on GitHub."
--          ]
--        , Html.label []
--          [ text "Client Id:"
--          ]
--        , Textfield.render (Mdl >> lift) [0,0,0,1] model.mdl
--          [ Options.onInput inputClientId
--          , Textfield.value model.clientId
--          ]
--          []
--        ,
--          Html.label []
--          [ text "Client Secret:"
--          ]
--        , Textfield.render (Mdl >> lift) [0,0,0,2] model.mdl
--          [ Options.onInput inputClientSecret
--          , Textfield.value model.clientSecret
--          ]
--          []
--        , Button.render (Mdl >> lift) [0,0,0,3] model.mdl
--          [ -- Options.onClick login
--          , Button.raised
--          , Button.accent
--          ]
--          [ text "Sign in"
--          ]
--        ]
--      ]
    ]
