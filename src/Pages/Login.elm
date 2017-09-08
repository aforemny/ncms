module Pages.Login exposing (..)

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
    , clientId : String
    , clientSecret : String
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , clientId = ""
    , clientSecret = ""
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | InputClientId String
    | InputClientSecret String


update lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model

        InputClientId clientId ->
            ( { model | clientId = clientId }, Cmd.none )

        InputClientSecret clientSecret ->
            ( { model | clientSecret = clientSecret }, Cmd.none )


view : (Msg msg -> msg) -> { login : { clientId : String, clientSecret : String } -> msg } -> Model -> Html msg
view lift { login } model =
    Html.div []
    [

      Card.view
      [ css "max-width" "600px"
      ]
      [
        Card.primary []
        [ Card.title [ Card.large ] [ text "Login" ]
        ]

      , Card.supportingText
        [ css "display" "flex"
        , css "flex-flow" "column"
        ]
        [
          Html.p []
          [ text "Obtain credentials by "
          , Html.a
            [ Html.href "https://github.com/settings/applications/new"
            ]
            [ text "registering an application" ]
          , text " on GitHub."
          ]
        , Html.label []
          [ text "Client Id:"
          ]
        , Textfield.render (Mdl >> lift) [0,0,0,1] model.mdl
          [ Options.onInput (InputClientId >> lift)
          , Textfield.value model.clientId
          ]
          []
        ,
          Html.label []
          [ text "Client Secret:"
          ]
        , Textfield.render (Mdl >> lift) [0,0,0,2] model.mdl
          [ Options.onInput (InputClientSecret >> lift)
          , Textfield.value model.clientSecret
          ]
          []
        , Button.render (Mdl >> lift) [0,0,0,3] model.mdl
          [ Options.onClick <|
                login
                  { clientId = model.clientId
                  , clientSecret = model.clientSecret
                  }
          , Button.raised
          , Button.accent
          ]
          [ text "Sign in"
          ]
        ]
      ]
    ]
