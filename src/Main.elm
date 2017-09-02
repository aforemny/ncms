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
import Regex
import Task

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
    }


type Msg
    = Mdl (Material.Msg Msg)

    | ApiMsg String (ApiMsg Msg)
    | Error Http.Error
    | Navigate String

    | Login
    | InputClientId String
    | InputClientSecret String
    | Authenticate String
    | UserProfile Github.User


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
    in
    ( { defaultModel
        | page = page
        , auth = flags.auth
        , accessToken = flags.accessToken
        , clientId = flags.clientId
        , clientSecret = flags.clientSecret
      }
    ,
      Cmd.batch
      [
        pageInit defaultModel page
        |> Cmd.batch

      , case (flags.accessToken, flags.auth) of
            ( Just accessToken, _ ) ->
                Cmd.batch
                [ Github.getUser accessToken
                  |> Task.attempt (\ result ->
                      case result of
                        Ok user -> UserProfile user
                        Err error -> Error error
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


type Page
    = Dashboard
    | Listing String
    | Edit String String
    | New String
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
                    New api
                (api::"edit"::id::_) ->
                    Edit api id
                (api::_) ->
                    Listing api
                _ ->
                    NotFound hash
        _ ->
            NotFound hash

toHash page =
    case page of
        Dashboard ->
            ""
        Listing api ->
            "#" ++ api
        New api ->
            "#" ++ api ++ "/new"
        Edit api id ->
            "#" ++ api ++ "/edit/" ++ id
        NotFound hash ->
            hash


pageInit : Model -> Page -> List (Cmd Msg)
pageInit model page =
    case page of
        Listing id ->
            let
                api =
                    Api.apis
                    |> List.filter (\ api -> api.type_ == id)
                    |> List.head
            in
            case api of
                Just { api } ->
                    [ api.list (handle Error (List >> ApiMsg id))
                        (Maybe.withDefault "" model.accessToken)
                        "aforemny"
                        "ncms"
                    ]
                Nothing ->
                    []

        Edit id obj ->
            let
                api =
                    Api.apis
                    |> List.filter (\ api -> api.type_ == id)
                    |> List.head
            in
            case api of
                Just { api } ->
                    [ api.get (handle Error (Get >> ApiMsg id))
                        (Maybe.withDefault "" model.accessToken)
                        "aforemny"
                        "ncms"
                        obj
                    ]
                Nothing ->
                    []
        _ ->
            []


subscriptions model =
    Sub.none


port redirect : String -> Cmd msg


port cacheAccessToken : String -> Cmd msg


port clearAccessToken : () -> Cmd msg


port cacheClientCredentials : { clientId : String, clientSecret : String, redirectUrl : Maybe String } -> Cmd msg


port clearClientCredentials : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        UserProfile user ->
            ( { model | user = Just user }, Cmd.none )

        Authenticate accessToken ->
            ( { model | accessToken = Just accessToken }
            ,
              Cmd.batch
              [ cacheAccessToken accessToken
              , Github.getUser accessToken
                |> Task.attempt (\ result ->
                    case result of
                      Ok user -> UserProfile user
                      Err error -> Error error
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

        Navigate hash ->
            let
                page =
                    fromHash hash
            in
            ( { model | page = page }
            ,
              if model.page == page then
                  Cmd.none
              else
                  Cmd.batch
                  [ Navigation.newUrl (if hash == "" then "#" else hash)
                  , Cmd.batch (pageInit model page)
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
                          clearAccessToken ()
                      _ ->
                          Cmd.none
              else
                  Cmd.none
            )

        ApiMsg id msg_ ->
            let
                api_ =
                    Api.apis
                    |> List.filter (\ api ->
                          if api.type_ == id then
                              True
                          else
                              False
                       )
                    |> List.head
            in
                case api_ of
                    Just api ->
                        let
                            ( apiModel_, effects ) =
                                apiUpdate (Maybe.withDefault "" model.accessToken) api (ApiMsg id) msg_ apiModel

                            apiModel =
                                Dict.get id model.apis
                                |> Maybe.withDefault defaultApiModel
                        in
                        ( { model | apis = Dict.insert id apiModel_ model.apis },
                          effects
                        )

                    Nothing ->
                        Debug.crash "no api"


-- apiUpdate :  -> (ApiMsg Msg -> Msg) -> ApiMsg Msg -> ApiModel -> ( ApiModel, Cmd Msg )
apiUpdate accessToken { type_, types, idField, api } lift msg model =
    let
        id value =
            "" -- TODO

        handle_ f =
            handle Error (f >> lift)

        value tehtype =
            case tehtype of
                Just { fields } ->
                    fields
                    |> List.map (\ ( fieldName, typeRep ) ->
                          let
                              fromString str =
                                  case typeRep of
                                      "String" ->
                                          Encode.string str
                                      "Maybe (String)" ->
                                          if str /= "" then
                                              Encode.string str
                                          else
                                              Encode.null
                                      "List (String)" ->
                                          Encode.list [Encode.string str]
                                      "Bool" ->
                                          Encode.bool <|
                                          if str == "True" then
                                              True
                                          else
                                              False
                                      _ ->
                                          Debug.crash "fromString"

                              fromValue typeRep value =
                                  case typeRep of
                                      "Bool" ->
                                          case value of
                                              Value.Bool v ->
                                                  Encode.bool v
                                              _ ->
                                                  Debug.crash "fromValue"
                                      "String" ->
                                          case value of
                                              Value.String v ->
                                                  Encode.string v
                                              _ ->
                                                  Debug.crash "fromValue"
                                      "List (String)" ->
                                          case value of
                                              Value.List vs ->
                                                  Encode.list <|
                                                  List.map (fromValue "String") vs
                                              _ ->
                                                  Debug.crash "fromValue"
                                      _ ->
                                          Encode.null
                          in
                          case Dict.get fieldName model.inputs of
                              Just strVal ->
                                  ( fieldName, fromString strVal )
                              Nothing ->
                                  case Value.expose model.value of
                                      Value.Object obj ->
                                          Dict.get fieldName obj
                                          |> Maybe.map (fromValue typeRep)
                                          |> Maybe.withDefault (fromString "")
                                          |> (,) fieldName
                                      _ ->
                                          ( fieldName, fromString "" )
                       )
                   |> Encode.object
                Nothing ->
                    Encode.null
    in
    case msg of
        Cancel ->
            ( model, Navigation.back 1 )

        List values ->
            ( { model | values = values }, Cmd.none )

        (Get value) ->
            ( { model | value = value }, Cmd.none )

        (Delete id) ->
            ( model, api.delete (handle_ (\_ -> DeleteOk)) accessToken "aforemny" "ncms" id )

        DeleteOk ->
            ( model, api.list (handle_ List) accessToken "aforemny" "ncms" )

        (Input fieldName fieldValue) ->
            ( { model | inputs = Dict.insert fieldName fieldValue model.inputs }, Cmd.none )

        (ApiMdl msg_) ->
            Material.update (ApiMdl >> lift) msg_ model

        Save ->
            let
                tehtype =
                    types
                    |> List.filter (\ t -> t.type_ == type_)
                    |> List.head

                value_ =
                    value tehtype
            in
            ( { model
                | value = value_
                , inputs = Dict.empty
              }
            ,
              api.create (handle_ SaveOk) accessToken "aforemny" "ncms" value_
            )

        SaveOk value ->
            ( model
            ,
              Task.perform (\ _ -> (Navigate (toHash (Listing type_)))) (Task.succeed ())
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
              |> List.map (\ api ->
                   Lists.li
                   [ Options.onClick (Navigate (toHash (Listing api.type_)))
                   , css "cursor" "pointer"
                   ]
                   [ Lists.text
                     [ css "padding-left" "36px"
                     ]
                     [ text api.type_
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

              New apiId ->

                  let
                      api =
                          Api.apis
                          |> List.filter (\ api -> api.type_ == apiId)
                          |> List.head
                  in
                  case api of
                      Just api ->
                          let
                              apiModel =
                                  Dict.get api.type_ model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          case List.head (List.filter (\ { type_ } -> type_ == apiId) api.types) of
                            Just type_ ->
                              editView (ApiMsg api.type_) type_ apiModel
                            Nothing ->
                              text "type not found"

                      Nothing ->
                          text "api not found"

              Edit apiId objId  ->

                  let
                      api =
                          Api.apis
                          |> List.filter (\ api -> api.type_ == apiId)
                          |> List.head
                  in
                  case api of
                      Just api ->
                          let
                              apiModel =
                                  Dict.get api.type_ model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          case List.head (List.filter (\ { type_ } -> type_ == apiId) api.types) of
                            Just type_ ->
                              editView (ApiMsg api.type_) type_ apiModel
                            Nothing ->
                              text "type not found"

                      Nothing ->
                          text "api not found"

              Listing id ->

                  let
                      api =
                          Api.apis
                          |> List.filter (\ api -> api.type_ == id)
                          |> List.head
                  in
                  case api of
                      Just api ->
                          let
                              apiModel =
                                  Dict.get api.type_ model.apis
                                  |> Maybe.withDefault defaultApiModel
                          in
                          listingView (ApiMsg api.type_) api apiModel

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


listingView lift api model =
  let
      rowStyle =
          [ cs "row"
          , css "display" "flex"
          , css "flex-flow" "row"
          , css "flex" "1 1 auto"
          ]

      listingType { type_, fields } =
           Html.div []
           [ Card.view
             [ css "width" "1200px"
             , css "max-width" "100%"
             ]
             [ Card.primary []
               [
                 Card.title [ Card.large ] [ text type_ ]
               ]

             , Card.supportingText []
               [ listingFields fields
               ]

             , Card.actions []
               [ Button.render (ApiMdl >> lift) [0,1,2,3] model.mdl
                 [ Options.onClick (Navigate (toHash (New type_)))
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
                 List.map (\ ( fieldName, _ ) -> fieldName) fields
               , Lists.divider [] []
               ]
             
             , model.values
               |> List.map (\ value ->
                      let
                          id =
                              case Value.expose value of
                                  Value.Object obj ->
                                      Dict.get api.idField obj
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
                              Options.onClick (Navigate ("#" ++ api.type_ ++ "/edit/" ++ id))
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
          |> List.map (\ ( fieldName, _ ) ->
                case value of
                    Value.Object kvs ->
                        Dict.get fieldName kvs
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
  ( List.concat
    [ 
      api.types
      |> List.map (\ type_ ->
             listingType type_
         )
    ]
  )


editView lift { type_, fields } model =
  Html.div
  [ Html.style
    [ ("font-size", "16px")
    , ("padding", "32px")
    ]
  ]
  [
    styled Html.h1 [ Typography.title ] [ text type_ ]

  , Card.view []
    [ Card.primary []
      [
        styled Html.div []
        ( fields
          |> List.concat << List.indexedMap (\ i ( fieldName, fieldType ) ->
              [ Html.label []
                [ text fieldName
                ]
              , case fieldType of
                    _ ->
                    Textfield.render (ApiMdl >> lift) [1,0,i] model.mdl
                        [ Options.onInput (Input fieldName >> lift)
                        , Textfield.value
                            ( Dict.get fieldName model.inputs
                              |> Maybe.withDefault
                                 ( case Value.expose model.value of
                                       (Value.Object obj) ->
                                           case Dict.get fieldName obj of
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
