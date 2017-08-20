module Main exposing (..)

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
import Material.Typography as Typography
import Navigation exposing (Location)
import Task

import Api
import Value


main : Program Never Model Msg
main =
    Navigation.program (Navigate << .hash)
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
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , apis = Dict.empty
    , error = Nothing
    , page = defaultPage
    }


type Msg
    = Mdl (Material.Msg Msg)

    | ApiMsg String (ApiMsg Msg)
    | Error Http.Error
    | Navigate String


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
    | DeleteOk Value


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            fromHash location.hash
    in
    ( { defaultModel | page = page }
    ,
      pageInit defaultModel page
      |> Cmd.batch
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
                    [ api.get (handle Error (Get >> ApiMsg id)) obj
                    ]
                Nothing ->
                    []
        _ ->
            []


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

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
                  [ Navigation.newUrl hash
                  , Cmd.batch (pageInit model page)
                  ]
            )

        Error error ->
            ( { model | error = Just error }, Cmd.none )

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
                                apiUpdate api (ApiMsg id) msg_ apiModel

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
apiUpdate { type_, types, idField, api } lift msg model =
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
            ( model, api.delete (handle_ DeleteOk) id )

        (DeleteOk value) ->
            ( model, api.list (handle_ List) )

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
                    |> Debug.log "value_"
            in
            ( { model
                | value = value_
                , inputs = Dict.empty
              }
            ,
              api.create (handle_ SaveOk) value_ )

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
    , css "min-height" "100vh"
    ]
    [ Drawer.render Mdl [0] model.mdl []
      [ Drawer.content []
        [ Lists.group []
          [
            Lists.subheader
            [ css "padding-left" "24px" ]
            [ text "Endpoints"
            ]
          , Lists.ul []
            ( Api.apis
              |> List.map (\ api ->
                   Lists.li
                   [ Options.on "click" (Decode.succeed (Navigate ("#" ++ api.type_)))
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

    , case model.page of
          Dashboard ->
              Html.div []
              [ text "Dashboard"
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
