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
import Regex
import Task
import Task exposing (Task)

import Api
import Page exposing (Page, ApiId(..), DataId(..))
import Pages.Dashboard
import Pages.Edit
import Pages.Listing
import Pages.Image
-- import Pages.Login
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
    , initialized : Bool

    , page : Page
    , edit : Pages.Edit.Model
    , listing : Pages.Listing.Model
    , dashboard : Pages.Dashboard.Model
    -- , login : Pages.Login.Model
    , image : Pages.Image.Model

    , queue : List (() -> Cmd Msg)
    , queue_ : List (() -> Task Http.Error Msg)
    , error : Maybe Http.Error
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel

    , apis = Dict.empty
    , initialized = True

    , page = Page.defaultPage
    , edit = Pages.Edit.defaultModel
    , listing = Pages.Listing.defaultModel
    , dashboard = Pages.Dashboard.defaultModel
    -- , login = Pages.Login.defaultModel
    , image = Pages.Image.defaultModel

    , queue = []
    , queue_ = []
    , error = Nothing
    }


type Msg
    = Mdl (Material.Msg Msg)

    | Error Http.Error
    | Navigate Page

    | EditMsg ApiId (Pages.Edit.Msg Msg)
    | ListingMsg ApiId (Pages.Listing.Msg Msg)
    | DashboardMsg (Pages.Dashboard.Msg Msg)
    -- | LoginMsg (Pages.Login.Msg Msg)
    | ImageMsg (Pages.Image.Msg Msg)

    -- | Login { clientId : String, clientSecret : String }

    | Enqueue (() -> Cmd Msg)
    | Enqueue_ (() -> Task Http.Error Msg)


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
  : {}
  -> Location
  -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            Page.fromHash location.hash

        ( model, effects ) =
            pageInit
                { defaultModel
                | page = page
                }
                page
    in
    ( model, effects )


pageInit : Model -> Page -> ( Model, Cmd Msg )
pageInit model page =
    case page of
        Page.Dashboard  ->
              let
                  ( dashboard, effects ) =
                      Pages.Dashboard.init DashboardMsg
                      {
                      }
              in
                  ( { model | dashboard = dashboard }, effects )

        Page.Listing (ApiId apiId) ->
            case Backend.lookup apiId Api.apis of
                Just api ->
                    let
                        ( listing, effects ) =
                            Pages.Listing.init (ListingMsg (ApiId apiId))
                                (getPage model api)
                    in
                        ( { model | listing = listing }, effects )
                Nothing ->
                    ( model, Cmd.none )

        Page.Edit (ApiId apiId) dataId ->
            case Backend.lookup apiId Api.apis of
                Just api ->
                    let
                        ( edit, effects ) =
                            Pages.Edit.init (EditMsg (ApiId apiId))
                                   (getPage model api)
                                   (Just dataId)
                    in
                        ( { model | edit = edit }, effects )
                Nothing ->
                    ( model, Cmd.none )

        Page.New (ApiId apiId) ->
            case Backend.lookup apiId Api.apis of
                Just api ->
                    let
                        ( edit, effects ) =
                            Pages.Edit.init (EditMsg (ApiId apiId))
                                (getPage model api)
                                Nothing
                    in
                        ( { model | edit = edit }, effects )
                Nothing ->
                    ( model, Cmd.none )

        Page.Image folder ->
            let
                ( image, effects ) =
                    Pages.Image.init ImageMsg { error = Error }
            in
                ( { model | image = image }, effects )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Material.subscriptions Mdl model
    , Pages.Image.subscriptions ImageMsg model.image
      -- TODO: other pages
    ]


port redirect : String -> Cmd msg


port cacheAccessToken : String -> Cmd msg


port clearAccessToken : () -> Cmd msg


port cacheClientCredentials : { clientId : String, clientSecret : String, redirectUrl : Maybe String } -> Cmd msg


port clearClientCredentials : () -> Cmd msg


enqueue : (() -> Cmd Msg) -> Cmd Msg
enqueue =
    Enqueue >> cmd


enqueue_ : (() -> Task Http.Error Msg) -> Cmd Msg
enqueue_ =
    Enqueue_ >> cmd


cmd : Msg -> Cmd Msg
cmd =
    Task.perform identity << Task.succeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Enqueue cmd ->
            if model.initialized then
                ( { model | queue = model.queue ++ [ cmd ] }, Cmd.none )
            else
                ( model, (cmd ()) )

        Enqueue_ task ->
            if model.initialized then
                ( model, Task.attempt (handle Error identity) (task ()) )
            else
                ( { model | queue_ = model.queue_ ++ [ task ] }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

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
            ( { model | error = Just error }, Cmd.none )

        DashboardMsg msg_ ->
            let
                ( dashboard, effects ) =
                    Pages.Dashboard.update DashboardMsg
                        msg_
                        model.dashboard
            in
            ( { model | dashboard = dashboard }, effects )

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

        ImageMsg msg_ ->
            let
                ( image, effects ) =
                    Pages.Image.update ImageMsg { error = Error }
                        msg_
                        model.image
            in
            ( { model | image = image }, effects )


handle fail succeed result =
    case result of
        Ok x -> succeed x
        Err e -> fail e


view : Model -> Html Msg
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
            , Lists.li
              [ Options.onClick (Navigate (Page.Image Nothing))
              , css "cursor" "pointer"
              , case model.page of
                    Page.Image _ ->
                        css "background-color" "#ccc"
                    _ ->
                        Options.nop
              ]
              [ Lists.text
                [ css "padding-left" "36px"
                ]
                [ text "Images"
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
            ( [ styled Html.div
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
                  Pages.Dashboard.view DashboardMsg
                      { -- login = Login
                      }
                      model.dashboard

              Page.New (ApiId apiId) ->
                  case Backend.lookup apiId Api.apis of
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

              Page.Image folder ->
                  Pages.Image.view ImageMsg
                      { navigate = Navigate
                      }
                      { folder = folder
                      }
                      model.image

              Page.NotFound _ ->
                  Html.div []
                  [ text "404"
                  ]
        ]
      ]
    ]
    |> Material.top


getPage :
    Model
    -> Backend.Rest Value
    -> { tipe : Backend.Tipe
       , error : Http.Error -> Msg
       , create : Value -> Task Http.Error ()
       , delete : String -> Task Http.Error ()
       , get : String -> Task Http.Error Value
       , list : Task Http.Error (List Value)
       , update : Value -> Task Http.Error ()
       }
getPage model api =
    { tipe = api.tipe
    , error = Error
    , create = api.create
    , update = api.update
    , delete = api.delete
    , get = api.get
    , list = api.list
    }
