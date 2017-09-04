port module Main exposing (..)

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

import Api
import Page exposing (Page, ApiId(..), DataId(..))
import Pages.Edit
import Pages.Listing
import Value


main =
    Navigation.programWithFlags (Navigate << Page.fromHash << .hash)
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


type alias Model =
    { mdl : Material.Model
    , apis : Dict String ApiModel
    , error : Maybe Http.Error
    , page : Page

    , edit : Pages.Edit.Model
    , listing : Pages.Listing.Model

    , clientId : String
    , clientSecret : String
    , auth : Maybe { code : String, state : String }
    , accessToken : Maybe String
    , user : Maybe Github.User
    , loginProcess : Bool

    , queue : List (String -> Cmd Msg)

    , repo : String
    , branch : String
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , apis = Dict.empty
    , error = Nothing
    , page = Page.defaultPage

    , edit = Pages.Edit.defaultModel
    , listing = Pages.Listing.defaultModel

    , clientId = ""
    , clientSecret = ""
    , auth = Nothing
    , accessToken = Nothing
    , user = Nothing
    , loginProcess = True

    , queue = []

    , repo = ""
    , branch = ""
    }


type Msg
    = Mdl (Material.Msg Msg)

    | Error Http.Error
    | Navigate Page

    | EditMsg ApiId (Pages.Edit.Msg Msg)
    | ListingMsg ApiId (Pages.Listing.Msg Msg)

    | Login
    | InputClientId String
    | InputClientSecret String
    | Authenticate String
    | UserProfile Github.User

    | Enqueue (String -> Cmd Msg)


type alias ApiModel =
    { mdl : Material.Model
    , value : Value
    , values : List Value

    , inputs : Dict String String
    }


defaultApiModel : ApiModel
defaultApiModel =
    { mdl = Material.defaultModel
    , value = Encode.null
    , values = []

    , inputs = Dict.empty
    }


init
  : { auth : Maybe { code : String
    , state : String }
    , accessToken : Maybe String
    , clientId : String
    , clientSecret : String
    }
  -> Location
  -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            Page.fromHash location.hash

        model =
            { defaultModel
            | page = page
            , auth = flags.auth
            , accessToken = flags.accessToken
            , clientId = flags.clientId
            , clientSecret = flags.clientSecret
            }

        ( modelWithPage, apiEffects ) =
            pageInit model page
    in
    ( modelWithPage
    ,
      Cmd.batch
      [
        apiEffects

      , case (flags.accessToken, flags.auth) of
            ( Just accessToken, _ ) ->
                Cmd.batch
                [ enqueue
                  ( \accessToken ->
                    Github.getUser accessToken
                    |> Task.attempt (\ result ->
                        case result of
                          Ok user -> UserProfile user
                          Err error -> Error error
                       )
                  )
                ]

            ( Nothing, Just { code, state } ) ->
                Http.send (handle Error Authenticate) <|
                Http.request
                  { method =
                      "POST"
                  , headers =
                      [ Http.header "Accept" "application/json"
                      ]
                  , url =
                      "https://cors-anywhere.herokuapp.com/https://github.com/login/oauth/access_token?client_id=" ++ flags.clientId ++ "&client_secret=" ++ flags.clientSecret ++ "&code=" ++ code ++ "&state=123"
                   , body =
                       Http.emptyBody
                   , expect =
                       Http.expectJson <|
                       Decode.at [ "access_token" ] Decode.string
                   , timeout=
                       Nothing
                   , withCredentials =
                       False
                  }
            _ ->
                Cmd.none
      ]
    )


pageInit : Model -> Page -> ( Model, Cmd Msg )
pageInit model page =
    case page of
        Page.Listing (ApiId apiId) ->
            case Backend.lookup apiId Api.apis of
                Nothing ->
                    ( model, Cmd.none )
                    -- ^ TODO
                Just api ->
                    let
                        ( listing, effects ) =
                            Pages.Listing.init (ListingMsg (ApiId apiId))
                                (getPage model api)
                    in
                        ( { model | listing = listing }, effects )

        Page.Edit (ApiId apiId) id ->
            case Backend.lookup apiId Api.apis of
                Nothing ->
                    ( model, Cmd.none  )
                    -- ^ TODO
                Just api ->
                    let
                        ( edit, effects ) =
                            Pages.Edit.init (EditMsg (ApiId apiId))
                                (getPage model api)
                                (Just id)
                    in
                        ( { model | edit = edit }, effects )

        Page.New (ApiId apiId) ->
            case Backend.lookup apiId Api.apis of
                Nothing ->
                    ( model, Cmd.none  )
                    -- ^ TODO
                Just api ->
                    let
                        ( edit, effects ) =
                            Pages.Edit.init (EditMsg (ApiId apiId))
                                (getPage model api)
                                Nothing
                    in
                        ( { model | edit = edit }, effects )

        _ ->
            ( model, Cmd.none )


subscriptions model =
    Sub.none


port redirect : String -> Cmd msg


port cacheAccessToken : String -> Cmd msg


port clearAccessToken : () -> Cmd msg


port cacheClientCredentials : { clientId : String, clientSecret : String, redirectUrl : Maybe String } -> Cmd msg


port clearClientCredentials : () -> Cmd msg


enqueue : (String -> Cmd Msg) -> Cmd Msg
enqueue =
    Enqueue >> cmd


cmd : Msg -> Cmd Msg
cmd =
    Task.perform identity << Task.succeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Enqueue cmd ->
            case model.accessToken of
                Nothing ->
                    ( { model | queue = model.queue ++ [ cmd ] }, Cmd.none )
                Just accessToken ->
                    ( model, (cmd accessToken) )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        UserProfile user ->
            ( { model | user = Just user }, Cmd.none )

        Authenticate accessToken ->
            ( { model | accessToken = Just accessToken, queue = [] }
            ,
              Cmd.batch
              [ cacheAccessToken accessToken
              , Cmd.batch (List.map (\cmd -> cmd accessToken) model.queue)
              , enqueue
                ( \accessToken ->
                  Github.getUser accessToken
                  |> Task.attempt (\ result ->
                      case result of
                        Ok user -> UserProfile user
                        Err error -> Error error
                     )
                )
              ]
            )

        Login ->
            ( model
            ,
              Cmd.batch
              [ cacheClientCredentials
                { clientId = model.clientId
                , clientSecret = model.clientSecret
                , redirectUrl = Just ("https://github.com/login/oauth/authorize?scope=repo&client_id=" ++ model.clientId ++ "&state=123")
                }
              ]
            )

        InputClientId clientId ->
          ( { model | clientId = clientId }, Cmd.none )

        InputClientSecret clientSecret ->
          ( { model | clientSecret = clientSecret }, Cmd.none )

        Navigate page ->
            if model.page == page then
                ( model, Cmd.none )
            else
                let
                    ( modelWithPage, apiEffects ) =
                        pageInit model page
                in
                ( { modelWithPage
                    | page = page
                  }
                ,
                  Cmd.batch
                  [ Navigation.newUrl (Page.toHash page)
                  , apiEffects
                  ]
                )

        Error error ->
            let
                _ = Debug.log "error" error
            in
            ( { model | error = Just error }
            ,
              if model.loginProcess then
                  case error of
                      Http.BadStatus _ ->
                          Cmd.none -- TODO:
                          -- clearAccessToken ()
                      _ ->
                          Cmd.none
              else
                  Cmd.none
            )

        EditMsg (ApiId apiId) msg_ ->
            case Backend.lookup apiId Api.apis of
                Nothing ->
                    ( model, Cmd.none )
                    -- ^ TODO: error

                Just api ->
                    let
                        ( edit, effects ) =
                            Pages.Edit.update (EditMsg (ApiId apiId))
                                ( getPage model api )
                                msg_
                                model.edit
                    in
                    ( { model | edit = edit }, effects )

        ListingMsg (ApiId apiId) msg_ ->
            case Backend.lookup apiId Api.apis of
                Nothing ->
                    ( model, Cmd.none )
                    -- ^ TODO: error

                Just api ->
                    let
                        ( listing, effects ) =
                            Pages.Listing.update (ListingMsg (ApiId apiId))
                                ( getPage model api )
                                msg_
                                model.listing
                    in
                    ( { model | listing = listing }, effects )


handle fail succeed result =
    case result of
        Ok x -> succeed x
        Err e -> fail e


view model =
    styled Html.div
    [ Typography.typography
    , css "display" "flex"
    , css "flex-flow" "row"
    ]
    [ Drawer.render Mdl [0] model.mdl []
      [
        Drawer.toolbarSpacer [] []

      , Drawer.content []
        [ Lists.group []
          [
            Lists.subheader
              [ css "padding-left" "24px"
              ]
              [ text "Ncms"
              ]

          , Lists.ul []
            [ Lists.li
              [ Options.onClick (Navigate Page.Dashboard)
              , css "cursor" "pointer"
              , when ( model.page == Page.Dashboard ) <|
                css "background-color" "#ccc"
              ]
              [ Lists.text
                [ css "padding-left" "36px"
                ]
                [ text "Dashboard"
                ]
              ]
            ]
          , 
            Lists.subheader
            [ css "padding-left" "24px"
            ]
            [ text "Endpoints"
            ]
          , Lists.ul []
            ( Api.apis
              |> List.map (\ { tipe } ->
                   Lists.li
                   [ Options.onClick (Navigate (Page.Listing (ApiId tipe.name)))
                   , css "cursor" "pointer"
                   , when ( model.page == Page.Listing (ApiId tipe.name) ) <|
                     css "background-color" "#ccc"
                   ]
                   [ Lists.text
                     [ css "padding-left" "36px"
                     ]
                     [ text tipe.name
                     ]
                   ]
                 )
            )
          ]
        ]
      ]

    , styled Html.div
      [ css "display" "flex"
      , css "flex-flow" "column"
      , css "flex-grow" "1"
      ]
      [ Toolbar.render Mdl [0] model.mdl []
        [ Toolbar.row []
          [ Toolbar.title []
            [ text "ncms"
            ]
          , Toolbar.section
            [ Toolbar.alignEnd
            ]
            [
            ]
          , Toolbar.section
            [ Toolbar.alignEnd
            ]
            ( case model.user of
                  Just user ->
                      [ Html.img
                        [ Html.src user.avatarUrl
                        , Html.style
                          [ ("width", "42px")
                          , ("height", "42px")
                          , ("border-radius", "21px")
                          , ("align-self", "center")
                          , ("margin-right", "16px")
                          ]
                        ]
                        []

                      , styled Html.div
                        [ css "align-self" "center"
                        , css "margin-right" "32px"
                        ]
                        [ Html.text user.name ]
                      ]

                  Nothing ->
                      [ styled Html.div
                        [ css "width" "42px"
                        , css "height" "42px"
                        , css "border-radius" "21px"
                        , css "align-self" "center"
                        , css "margin-right" "16px"
                        , css "background-color" "#ccc"
                        ]
                        []

                      , styled Html.div
                        [ css "align-self" "center"
                        , css "margin-right" "32px"
                        , css "background-color" "#ccc"
                        , css "height" "24px"
                        , css "border-radius" "12px"
                        , css "width" "160px"
                        ]
                        []
                      ]
            )
          ]
        ]

      , styled Html.div
        [ css "padding-left" "36px"
        ]
        [ case model.page of
              Page.Dashboard ->
                  Html.div []
                  [
                    styled Html.h1 [ Typography.title ] [ text "Dashboard" ]

                  , case model.accessToken of
                        Nothing ->
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
                              , Textfield.render Mdl [0,0,0,1] model.mdl
                                [ Options.onInput InputClientId
                                , Textfield.value model.clientId
                                ]
                                []
                              ,
                                Html.label []
                                [ text "Client Secret:"
                                ]
                              , Textfield.render Mdl [0,0,0,2] model.mdl
                                [ Options.onInput InputClientSecret
                                , Textfield.value model.clientSecret
                                ]
                                []
                              , Button.render Mdl [0,0,0,3] model.mdl
                                [ Options.onClick Login
                                , Button.raised
                                , Button.accent
                                ]
                                [ text "Sign in"
                                ]
                              ]
                            ]
                        Just _ ->
                            Card.view
                            [ css "max-width" "900px"
                            ]
                            [ text (toString model)
                            ]
                  ]

              Page.New (ApiId apiId) ->

                  let
                      api =
                          Backend.lookup apiId Api.apis
                  in
                  case api of
                      Just { tipe } ->
                          let
                              apiModel =
                                  Dict.get tipe.name model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          Pages.Edit.view True (EditMsg (ApiId tipe.name)) tipe model.edit

                      Nothing ->
                          text "api not found"

              Page.Edit (ApiId apiId) (DataId objId) ->
                  case Backend.lookup apiId Api.apis of
                      Just { tipe } ->
                          let
                              apiModel =
                                  Dict.get tipe.name model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          Pages.Edit.view False (EditMsg (ApiId tipe.name)) tipe model.edit

                      Nothing ->
                          text "api not found"

              Page.Listing (ApiId apiId) ->
                  case Backend.lookup apiId Api.apis of
                      Just { tipe } ->
                          let
                              apiModel =
                                  Dict.get tipe.name model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          Pages.Listing.view (ListingMsg (ApiId tipe.name)) { navigate = Navigate } tipe model.listing

                      Nothing ->
                          text "api not found"

              Page.NotFound _ ->
                  Html.div []
                  [ text "404"
                  ]
        ]
      ]
    ]
    |> Material.top


--getPage :
--    Model
--    -> Backend.Rest Value
--    -> { tipe : Backend.Tipe
--       , error : Http.Error -> msg
--       , create : (Result Http.Error Value -> msg) -> Value -> Cmd msg
--       , delete : (Result Http.Error () -> msg) -> String -> Cmd msg
--       , get : (Result Http.Error Value -> msg) -> String -> Cmd msg
--       , list : (Result Http.Error (List Value) -> msg) -> Cmd msg
--       , update : (Result Http.Error Value -> msg) -> Value -> Cmd msg
--       }
getPage model api =
    let
      owner =
          "aforemny"

      repo =
          "ncms"

      tipe = api.tipe

      error = Error

      create cont value =
          let
              f accessToken =
                  api.create cont accessToken owner repo value
          in
          model.accessToken
          |> Maybe.map f
          |> Maybe.withDefault (enqueue f)

      update cont value =
          let
              f accessToken =
                  api.update cont accessToken owner repo value
          in
          model.accessToken
          |> Maybe.map f
          |> Maybe.withDefault (enqueue f)

      delete cont id =
          let
              f accessToken =
                  api.delete cont accessToken owner repo id
          in
          model.accessToken
          |> Maybe.map f
          |> Maybe.withDefault (enqueue f)

      get cont id =
          let
              f accessToken =
                  api.get cont accessToken owner repo id
          in
          model.accessToken
          |> Maybe.map f
          |> Maybe.withDefault (enqueue f)

      list cont =
          let
              f accessToken =
                  api.list cont accessToken owner repo
          in
          model.accessToken
          |> Maybe.map f
          |> Maybe.withDefault (enqueue f)
    in
    { tipe = tipe
    , error = error
    , create = create
    , update = update
    , delete = delete
    , get = get
    , list = list
    }
