module Main exposing (..)

import Api.Users as Users exposing (User)
import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Http
import Json.Decode as Json


main =
    Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


type alias Model =
    { user : User
    , users : List User
    , error : Maybe Http.Error
    }


defaultModel : Model
defaultModel =
    { user = Users.defaultUser
    , users = []
    , error = Nothing
    }


type Msg
    = List (List User)
    -- | Get User
    | Error Http.Error
    | SetUsername String
    | SetEmail String
    | SetAvatar String
    | Create
    | CreateOk User
    | Update
    | UpdateOk User
    | Show User
    | ShowOk User
    | Edit User
    | EditOk User
    | Delete User
    | DeleteOk User


init =
    ( defaultModel,
      Cmd.batch
      [ Users.list (handle Error List)
      ]
    )


subscriptions model =
    Sub.none


update msg model =
    let
        updateUser fun model =
            { model | user = fun model.user }
    in
    case msg of
        List users ->
            ( { model | users = users }, Cmd.none )

        Error error ->
            ( { model | error = Just error }, Cmd.none )

--        Get user ->
--            ( { model | user = user }, Cmd.none )

        Update ->
            ( model,
                Users.update (handle Error UpdateOk) model.user
            )

        UpdateOk user ->
            ( { model | user = user },
                Users.list (handle Error List)
            )

        Create ->
            ( model,
                Users.create (handle Error CreateOk) model.user
            )

        CreateOk user ->
            ( { model | user = user },
                Users.list (handle Error List)
            )

        SetUsername username ->
            ( model
              |> updateUser (\ user -> { user | username = username })
            , Cmd.none
            )

        SetEmail email ->
            ( model
              |> updateUser (\ user -> { user | email = email })
            , Cmd.none
            )

        SetAvatar avatar ->
            ( model
              |> updateUser (\ user -> { user | avatar = Just avatar })
            , Cmd.none
            )

        Show user ->
            ( model, Users.get (handle Error ShowOk) user.username )

        ShowOk user ->
            ( { model | user = user }, Cmd.none )

        Edit user ->
            ( model, Users.get (handle Error EditOk) user.username )

        EditOk user ->
            ( { model | user = user }, Cmd.none )

        Delete user ->
            ( model, Users.delete (handle Error DeleteOk) user.username )

        DeleteOk user ->
            ( model, Users.list (handle Error List) )


handle fail succeed result =
    case result of
        Ok x -> succeed x
        Err e -> fail e


view model =
    let
        userRow options =
            Html.div
            ( Html.style
              [ ("display", "flex")
              , ("flex-flow", "row")
              , ("flex", "1 1 25%")
              , ("padding", "8px 24px")
              , ("margin-bottom", "4px")
              , ("background-color", "#eee")
              ]
            :: options
            )

        userCard options =
            Html.div
            ( Html.style
              [ ("display", "flex")
              , ("flex-flow", "row")
              ]
            :: options
            )

        card title options nodes =
            Html.div
            ( Html.style
              [ ("display", "flex")
              , ("flex-flow", "column")
              , ("margin-top", "48px")
              , ("margin-bottom", "48px")
              ]
            :: options
            )
            [ Html.h2 [] [ text title ]
            , Html.div
              [ Html.style
                [ ("border", "1px solid #ccc")
                , ("padding", "24px")
                ]
              ]
              nodes 
            ]
    in
    Html.div
    [ Html.style
      [ ("font-size", "16px")
      , ("padding", "32px")
      ]
    ]
    [ card "Listing" []
      [ Html.div
        [ Html.style
          [ ("display", "flex")
          , ("flex-flow", "column")
          , ("max-width", "900px")
          ]
        ]
        ( model.users
          |> List.map (\ user ->
                let
                    column options =
                        Html.div
                        ( Html.style
                          [ ("width", "25%")
                          ]
                        :: Html.style [ ("color", "#777") ]
                        :: options
                        )

                    button options =
                        Html.a
                        ( Html.style
                          [ ("cursor", "pointer")
                          ]
                        :: Html.style [ ("color", "#00f") ]
                        :: options
                        )
                in
                userRow []
                [ column
                  [ Html.style [ ("color", "#000") ]
                  ]
                  [ text user.username
                  ]
                , column []
                  [ text user.email
                  ]
                , column []
                  [ text (Maybe.withDefault "–" (if user.avatar == Just "" then Nothing else user.avatar))
                  ]
                , column []
                  [ button [ Html.on "click" (Json.succeed (Show user)) ] [ text "show" ]
                  , text " "
                  , button [ Html.on "click" (Json.succeed (Edit user)) ] [ text "edit" ]
                  , text " "
                  , button [ Html.on "click" (Json.succeed (Delete user)) ] [ text "delete" ]
                  ]
                ]
             )
        )
      ]

    , card "Show / Edit / Create"
      [ Html.style
        [ ("display", "flex")
        , ("flex-flow", "column wrap")
        , ("max-width", "300px")
        ]
      ]
      [ Html.input
        [ Html.value model.user.username
        , Html.on "change" (Json.map SetUsername Html.targetValue)
        , Html.placeholder "Username"
        ]
        []
      , Html.input
        [ Html.value model.user.email
        , Html.on "change" (Json.map SetEmail Html.targetValue)
        , Html.placeholder "E-Mail"
        ]
        []
      , Html.input
        [ Html.value (Maybe.withDefault "" model.user.avatar)
        , Html.on "change" (Json.map SetAvatar Html.targetValue)
        , Html.placeholder "Avatar"
        ]
        []
      , Html.button
        [ Html.on "click" (Json.succeed Update)
        ]
        [ text "Update"
        ]
      , Html.button
        [ Html.on "click" (Json.succeed Create)
        ]
        [ text "Create"
        ]
      ]
    ]
