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
import Ncms.Github as Github
import Ncms.Backend as Backend
import Regex
import Task
import Task exposing (Task)

import Api
import Value


main =
    Navigation.programWithFlags (Navigate << .hash)
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

    , clientId : String
    , clientSecret : String
    , auth : Maybe { code : String, state : String }
    , accessToken : Maybe String
    , user : Maybe Github.User
    , loginProcess : Bool

    , queue : List (Cmd Msg)

    , repo : String
    , branch : String
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , apis = Dict.empty
    , error = Nothing
    , page = defaultPage

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

    | ApiMsg ApiId (ApiMsg Msg)
    | Error Http.Error
    | Navigate String

    | Login
    | InputClientId String
    | InputClientSecret String
    | Authenticate String
    | UserProfile Github.User

    | Enqueue (List (Cmd Msg))


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


type ApiMsg m
    = List (List Value)
    | Input String String
    | ApiMdl (Material.Msg m)
    | Get Value
    | Save
    | SaveOk Value
    | Cancel
    | Delete String
    | DeleteOk


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
            fromHash location.hash

        model =
            { defaultModel
            | page = page
            , auth = flags.auth
            , accessToken = flags.accessToken
            , clientId = flags.clientId
            , clientSecret = flags.clientSecret
            }

        ( apiModel, apiEffects ) =
            pageInit model page
    in
    ( { model
        | apis =
            case apiModel of
                Just (apiId, apiModel) ->
                    Dict.insert apiId apiModel model.apis
                Nothing ->
                    model.apis
      }
    ,
      Cmd.batch
      [
        apiEffects

      , case (flags.accessToken, flags.auth) of
            ( Just accessToken, _ ) ->
                Cmd.batch
                [ enqueue
                  [ Github.getUser accessToken
                    |> Task.attempt (\ result ->
                        case result of
                          Ok user -> UserProfile user
                          Err error -> Error error
                       )
                  ]
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


type ApiId =
    ApiId String


type DataId =
    DataId String


type Page
    = Dashboard
    | Listing ApiId
    | Edit ApiId DataId
    | New ApiId
    | NotFound String


defaultPage =
    Dashboard


fromHash hash =
    case String.uncons hash of
        Nothing ->
            Dashboard
        Just ('#', "") ->
            Dashboard
        Just ('#', rest) ->
            case String.split "/" rest of
                (api::"new"::_) ->
                    New (ApiId api)
                (api::"edit"::id::_) ->
                    Edit (ApiId api) (DataId id)
                (api::_) ->
                    Listing (ApiId api)
                _ ->
                    NotFound hash
        _ ->
            NotFound hash

toHash page =
    case page of
        Dashboard ->
            ""
        Listing (ApiId api) ->
            "#" ++ api
        New (ApiId api) ->
            "#" ++ api ++ "/new"
        Edit (ApiId api) (DataId id) ->
            "#" ++ api ++ "/edit/" ++ id
        NotFound hash ->
            hash


pageInit : Model -> Page -> (Maybe ( String, ApiModel ), Cmd Msg )
pageInit model page =
    case page of
        Listing (ApiId apiId) ->
            case Backend.lookup apiId Api.apis of
                Just { list } ->
                    ( Just (apiId, defaultApiModel)
                    ,
                      enqueue
                      [ list (handle Error (List >> ApiMsg (ApiId apiId)))
                          (Maybe.withDefault "" model.accessToken)
                          "aforemny"
                          "ncms"
                      ]
                    )
                Nothing ->
                    ( Just (apiId, defaultApiModel), Cmd.none )

        Edit (ApiId apiId) (DataId id) ->
            case Backend.lookup apiId Api.apis of
                Just { get } ->
                    ( Just (apiId, defaultApiModel)
                    , 
                      enqueue
                      [ get (handle Error (Get >> ApiMsg (ApiId apiId)))
                        (Maybe.withDefault "" model.accessToken)
                        "aforemny"
                        "ncms"
                        id
                      ]
                    )
                Nothing ->
                    ( Just (apiId, defaultApiModel), Cmd.none )
        _ ->
            ( Nothing, Cmd.none )


subscriptions model =
    Sub.none


port redirect : String -> Cmd msg


port cacheAccessToken : String -> Cmd msg


port clearAccessToken : () -> Cmd msg


port cacheClientCredentials : { clientId : String, clientSecret : String, redirectUrl : Maybe String } -> Cmd msg


port clearClientCredentials : () -> Cmd msg


enqueue : List (Cmd Msg) -> Cmd Msg
enqueue =
    Enqueue >> cmd


cmd : Msg -> Cmd Msg
cmd msg =
    Task.perform identity (Task.succeed msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Enqueue cmds ->
            if model.accessToken == Nothing then
                ( { model | queue = model.queue ++ cmds }, Cmd.none )
            else
                ( model, Cmd.batch cmds )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        UserProfile user ->
            ( { model | user = Just user }, Cmd.none )

        Authenticate accessToken ->
            ( { model | accessToken = Just accessToken, queue = [] }
            ,
              Cmd.batch
              [ cacheAccessToken accessToken
              , Cmd.batch model.queue
              , enqueue
                [ Github.getUser accessToken
                  |> Task.attempt (\ result ->
                      case result of
                        Ok user -> UserProfile user
                        Err error -> Error error
                     )
                ]
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

        Navigate hash ->
            let
                page =
                    fromHash hash

                ( apiModel, apiEffects ) =
                    pageInit model page
            in
            if model.page == page then
                ( { model | page = page }, Cmd.none )
            else
                ( { model
                    | page = page
                    , apis =
                        case apiModel of
                            Just (apiId, apiModel) ->
                                Dict.insert apiId apiModel model.apis
                            Nothing ->
                                model.apis
                  }
                ,
                  Cmd.batch
                  [ Navigation.newUrl (if hash == "" then "#" else hash)
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

        ApiMsg (ApiId apiId) msg_ ->
            case Backend.lookup apiId Api.apis of
                Just api ->
                    let
                        ( apiModel_, effects ) =
                            apiUpdate (Maybe.withDefault "" model.accessToken) api (ApiMsg (ApiId apiId)) msg_ apiModel

                        apiModel =
                            Dict.get apiId model.apis
                            |> Maybe.withDefault defaultApiModel
                    in
                    ( { model | apis = Dict.insert apiId apiModel_ model.apis },
                      effects
                    )

                Nothing ->
                    Debug.crash "no api"


apiUpdate
    : String
    -> Backend.Rest Msg
    -> (ApiMsg Msg -> Msg)
    -> ApiMsg Msg
    -> ApiModel
    -> ( ApiModel, Cmd Msg )
apiUpdate accessToken { tipe, delete, get, list, update, create } lift msg model =
    let
        id value =
            "" -- TODO

        handle_ f =
            handle Error (f >> lift)
    in
    case msg of
        Cancel ->
            ( model, Navigation.back 1 )

        List values ->
            ( { model | values = values }, Cmd.none )

        (Get value) ->
            ( { model
                | inputs = Backend.toInputs tipe value
                , value = value
              }
            ,
              Cmd.none
            )

        (Delete id) ->
            ( model, enqueue [ delete (handle_ (\_ -> DeleteOk)) accessToken "aforemny" "ncms" id ] )

        DeleteOk ->
            ( model, enqueue [ list (handle_ List) accessToken "aforemny" "ncms" ] )

        (Input fieldName fieldValue) ->
            ( { model | inputs = Dict.insert fieldName fieldValue model.inputs }, Cmd.none )

        (ApiMdl msg_) ->
            Material.update (ApiMdl >> lift) msg_ model

        Save ->
            let
                value =
                    Backend.toValue tipe model.inputs
            in
            ( { model
                | value = value
                , inputs = Dict.empty
              }
            ,
              enqueue
              [ create (handle_ SaveOk) accessToken "aforemny" "ncms" value
              ]
            )

        SaveOk value ->
            ( model
            ,
              Task.perform (\ _ -> (Navigate (toHash (Listing (ApiId tipe.name))))) (Task.succeed ())
            )


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
              [ Options.onClick (Navigate (toHash Dashboard))
              , css "cursor" "pointer"
              , when ( model.page == Dashboard ) <|
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
                   [ Options.onClick (Navigate (toHash (Listing (ApiId tipe.name))))
                   , css "cursor" "pointer"
                   , when ( model.page == Listing (ApiId tipe.name) ) <|
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
      [ Toolbar.view []
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
              Dashboard ->
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

              New (ApiId apiId) ->

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
                          editView True (ApiMsg (ApiId tipe.name)) tipe apiModel

                      Nothing ->
                          text "api not found"

              Edit (ApiId apiId) (DataId objId) ->
                  case Backend.lookup apiId Api.apis of
                      Just { tipe } ->
                          let
                              apiModel =
                                  Dict.get tipe.name model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          editView False (ApiMsg (ApiId tipe.name)) tipe apiModel

                      Nothing ->
                          text "api not found"

              Listing (ApiId apiId) ->
                  case Backend.lookup apiId Api.apis of
                      Just { tipe } ->
                          let
                              apiModel =
                                  Dict.get tipe.name model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          listingView (ApiMsg (ApiId tipe.name)) tipe apiModel

                      Nothing ->
                          text "api not found"

              NotFound _ ->
                  Html.div []
                  [ text "404"
                  ]
        ]
      ]
    ]
    |> Material.top


listingView lift tipe model =
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
                 Card.title [ Card.large ] [ text (tipe.name ++ " listing") ]
               ]

             , Card.supportingText []
               [ listingFields ( idField :: fields )
               ]

             , Card.actions []
               [ Button.render (ApiMdl >> lift) [0,1,2,3] model.mdl
                 [ Options.onClick (Navigate (toHash (New (ApiId tipe.name))))
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
                              Options.onClick (Navigate (toHash (Edit (ApiId tipe.name) (DataId id))))
                          Nothing ->
                              Options.nop
                    , cs "material-icons"
                    ]
                    [ text "edit"
                    ]
                  , styled Html.i
                    [ case id of
                          Just id ->
                              Options.onClick (lift (Delete id))
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


editView isCreate lift { name, idField, fields } model =
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
                                  |> Maybe.withDefault
                                     ( case Value.expose model.value of
                                           (Value.Object obj) ->
                                               case Dict.get field.name obj of
                                                   Just Value.Null ->
                                                      ""
                                                   Just (Value.Bool v) ->
                                                      toString v
                                                   Just (Value.String v) ->
                                                      ""
                                                   Just (Value.Number v) ->
                                                      ""
                                                   Just (Value.List vs) ->
                                                      ""
                                                   Just (Value.Object obj) ->
                                                      ""
                                                   Nothing ->
                                                       ""
                                           _ ->
                                               ""
                                     )
                            in
                            Checkbox.render (ApiMdl >> lift) [1,0,i] model.mdl
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
                        Textfield.render (ApiMdl >> lift) [1,0,i] model.mdl
                            [ Options.onInput (Input field.name >> lift)
                            , Textfield.value
                                ( Dict.get field.name model.inputs
                                  |> Maybe.withDefault
                                     ( case Value.expose model.value of
                                           (Value.Object obj) ->
                                               case Dict.get field.name obj of
                                                   Just Value.Null ->
                                                      ""
                                                   Just (Value.Bool v) ->
                                                      toString v
                                                   Just (Value.String v) ->
                                                      v
                                                   Just (Value.Number v) ->
                                                      toString v
                                                   Just (Value.List vs) ->
                                                      toString vs
                                                   Just (Value.Object obj) ->
                                                      toString obj
                                                   Nothing ->
                                                       ""
                                           _ ->
                                               ""
                                     )
                                )
                            , Textfield.fullWidth
                            , css "margin-bottom" "32px"
                            , when ( ( field.name == idField.name ) && not isCreate) <|
                              Textfield.disabled
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
      [ Button.render (ApiMdl >> lift) [1,1,0] model.mdl
        [ Options.onClick (lift Save)
        , Button.primary
        ]
        [ text "Save"
        ]
      , Button.render (ApiMdl >> lift) [1,1,0] model.mdl
        [ Options.onClick (lift Cancel)
        ]
        [ text "Cancel"
        ]
      ]
    ]
  ]
