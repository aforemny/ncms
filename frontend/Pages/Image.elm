module Pages.Image exposing (..)

import Api
import Dict exposing (Dict)
import FileReader
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Http
import Http
import Json.Decode as Decode exposing (Value)
import Json.Decode as Json
import Json.Decode as Json
import Json.Encode as Encode
import Material
import Material.Button as Button
import Material.Menu as Menu
import Material.Card as Card
import Material.Checkbox as Checkbox
import Material.Drawer.Permanent as Drawer
import Material.Elevation as Elevation
import Material.GridList as GridList
import Material.List as Lists
import Material.Options as Options exposing (styled, css, cs, when)
import Material.Textfield as Textfield
import Material.Theme as Theme
import Material.Toolbar as Toolbar
import Material.Typography as Typography
import Navigation exposing (Location)
import Ncms.Backend as Backend
import Ncms.Github as Github
import Page exposing (Page)
import Regex
import Set exposing (Set)
import Task exposing (Task)


type alias Model =
    { mdl : Material.Model
    , images : List Image
    , folder : Maybe String
    , uploadingImages
        : List
          { nativeFile : FileReader.NativeFile
          , dataUrl : Maybe String
          , failed : Bool
          , uploading : Bool
          }
    }


defaultModel : Model
defaultModel =
    { mdl = Material.defaultModel
    , images = []
    , folder = Nothing
    , uploadingImages = []
    }


type Msg msg
    = Mdl (Material.Msg msg)
    | Init (List Image)
    | Select String (List FileReader.NativeFile)
    | Load String FileReader.NativeFile FileReader.FileContentDataUrl
    | Error FileReader.NativeFile FileReader.Error
    | Upload FileReader.NativeFile
    | Folder String
    | Delete Image
    | DeleteOk Image


subscriptions lift model =
    Material.subscriptions (lift << Mdl) model


update
    : (Msg msg -> msg)
    -> { error : Http.Error -> msg
       }
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift { error } msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model

        Init images ->
            ( { model | images = images }, Cmd.none )

        Error nativeFile error ->
            let
                _ = Debug.log "error" error
            in
            ( model, Cmd.none )

        Select folder nativeFiles ->
            ( { model
                  | uploadingImages =
                        (++) model.uploadingImages
                        ( nativeFiles
                          |> List.map (\ nativeFile ->
                                 { nativeFile = nativeFile
                                 , dataUrl = Nothing
                                 , failed = False
                                 , uploading = False
                                 }
                             )
                        )
              }
            ,
              nativeFiles
              |> List.map (\ nativeFile ->
                     attempt (lift << Error nativeFile) (lift << Load folder nativeFile) <|
                     FileReader.readAsDataUrl nativeFile.blob
                 )
              |> Cmd.batch
            )

        Load folder nativeFile fileContentDataUrl ->
            ( { model
                  | uploadingImages =
                        model.uploadingImages
                        |> List.map (\ uploadingImage ->
                               if uploadingImage.nativeFile == nativeFile then
                                   let
                                      dataUrl =
                                         Json.decodeValue Json.string fileContentDataUrl
                                         |> Result.toMaybe
                                   in
                                   { nativeFile = nativeFile
                                   , dataUrl = dataUrl
                                   , failed = dataUrl /= Nothing
                                   , uploading = dataUrl /= Nothing
                                   }
                               else
                                   uploadingImage
                           )
              }
            ,
              attempt error (\_ -> lift (Upload nativeFile)) <|
              Http.toTask <|
              Http.request
              { method = "POST"
              , headers = []
              , url = "image/" ++ folder ++ "/" ++ nativeFile.name
              , expect = Http.expectString
              , body = FileReader.rawBody "image/png" nativeFile
              , timeout = Nothing
              , withCredentials = False
              }
            )

        Upload nativeFile ->
            ( { model
                  | uploadingImages =
                        model.uploadingImages
                        |> List.map (\ uploadingImage ->
                               if uploadingImage.nativeFile == nativeFile then
                                   { uploadingImage
                                         | uploading = False
                                   }
                               else
                                   uploadingImage
                           )
              }
            ,
              Cmd.none
            )

        Folder folder ->
            ( { model | folder = Just folder }, Cmd.none )

        Delete image ->
            let
                url =
                    image.directory ++ "/" ++ image.file ++ "." ++ image.extension
            in
              ( model
              , 
                attempt error (\_ -> lift (DeleteOk image)) <|
                Http.toTask <|
                Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , expect = Http.expectString
                , body = Http.emptyBody
                , timeout = Nothing
                , withCredentials = False
                }
              )

        DeleteOk image ->
            ( { model | images = List.filter ((/=) image) model.images }, Cmd.none )


handle error succeed result =
    case result of
        Err e -> error e
        Ok x -> succeed x

init
    : (Msg msg -> msg)
    -> { error : Http.Error -> msg
       }
    -> ( Model, Cmd msg )
init lift { error } =
    ( defaultModel,
      attempt error (lift << Init) <|
      Http.toTask <|
      Http.request
      { method = "GET"
      , headers = []
      , url = "image"
      , body = Http.emptyBody
      , expect = Http.expectJson (Json.list decodeImage)
      , timeout = Nothing
      , withCredentials = False
      }
    )


attempt fail cont =
    Task.attempt
        ( \ result ->
            case result of
                Err e -> fail e
                Ok x -> cont x
        )


view
    : (Msg msg -> msg)
    -> { navigate : Page -> msg }
    -> { folder : Maybe String }
    -> Model
    -> Html msg
view lift { navigate } { folder } model =
    Html.div []
    [
      styled Html.h1 [ Typography.title ] [ text "Images" ]

    , styled Html.div
      [ css "display" "flex"
      , css "flex-flow" "row"
      ]
      [
        let
            imageDirectories =
                model.images
                |> List.map .directory
                |> Set.fromList
                |> Set.toList
        in
        styled Html.div
        [ css "flex" "0 0 auto"
        , css "background-color" "#ccc"
        , css "padding" "24px"
        , css "margin-right" "24px"
        ]
        ( imageDirectories
          |> List.map (\ imageDirectory_ ->
                 let
                     imageDirectory =
                         imageDirectory_
                         |> String.split "/"
                         |> Maybe.withDefault [] << List.tail
                         |> String.join "/"
                 in
                 styled Html.div
                 [ Options.onClick (navigate (Page.Image (Just imageDirectory)))
                 , when (Just imageDirectory == folder) <|
                   css "font-weight" "bold"
                 ]
                 [ text imageDirectory
                 ]
             )
        )


      , styled Html.div
        [ css "flex" "1 1 100%"
        ]
        [ Html.input
          [ Html.type_ "file"
          , Html.multiple True
          , FileReader.onFileChange (Select (Maybe.withDefault "" folder) >> lift)
          ]
          [
          ]

        , Card.view []
          ( if model.images == [] then
                []
            else
                [
                  GridList.render (lift << Mdl) [0] model.mdl
                  [ GridList.twolineCaption
                  , GridList.iconAlignEnd
                  ]
                  ( List.concat
                    [
                      model.uploadingImages
                      |> List.map (\ { nativeFile, dataUrl } ->
                             GridList.tile []
                             [ GridList.primary []
                               ( case dataUrl of
                                     Just url ->
                                         [ GridList.image [] url
                                         ]
                                     Nothing ->
                                         []
                               )
                             ]
                         )

                    , model.images
                      |> List.filterMap ( \ image ->
                             case folder of
                                 Just folder ->
                                     if String.startsWith ("image/" ++ folder) image.directory then
                                         Just image
                                     else
                                         Nothing
                                 Nothing ->
                                     Just image
                         )
                      |> ( if folder /= Nothing then
                               identity
                           else
                               List.take 24
                         )
                      |> List.indexedMap (\ i image ->
                             let
                                url =
                                    image.directory ++ "/" ++ image.file ++ "." ++ image.extension
                             in
                             GridList.tile []
                             [ GridList.primary []
                               [ GridList.primaryContent
                                 [ css "background-image" ("url(\"" ++ url ++ "\")")
                                 ]
                                 []
                               ]
                             , GridList.secondary
                               [
                               ]
                               [ styled Html.div
                                 [ cs "mdc-grid-tile__icon"
                                 ]
                                 [ styled Html.i
                                   [ cs "material-icons"
                                   , Menu.attach (Mdl >> lift) [0,i]
                                   ]
                                   [ text "more_vert"
                                   ]

                                 , Menu.render (Mdl >> lift) [0,i] model.mdl
                                   [ Menu.openFromTopRight
                                   ]
                                   ( Menu.ul Lists.ul []
                                     [ Menu.li Lists.li
                                       [ Options.onClick (lift (Delete image))
                                       ]
                                       [ text "Delete"
                                       ]
                                     ]
                                   )
                                 ]
                               , GridList.title []
                                 [ text image.file
                                 ]
                               , GridList.supportingText []
                                 [ text image.size
                                 ]
                               ]
                             ]
                         )
                    ]
                  )
                ]
          )
        ]
      ]
    ]


-- IMAGE BACKEND:


type alias Image =
    { size : String
    , directory : String
    , extension : String
    , file : String
    , width : Int
    , height : Int
    }


decodeImage =
    Decode.map6 Image
      (Decode.at [ "size" ] Decode.string)
      (Decode.at [ "directory" ] Decode.string)
      (Decode.at [ "extension" ] Decode.string)
      (Decode.at [ "file" ] Decode.string)
      (Decode.at [ "width" ] Decode.int)
      (Decode.at [ "height" ] Decode.int)
