module Main where

import Core
import Core.Directory as Directory
import Parser (Module(..), ApiDecl(..), TypeDecl(..), TypeRep(..))
import qualified CodeGen
import qualified Core.List as List
import qualified Core.Maybe as Maybe
import qualified Core.String as String
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as LB
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Image
import qualified Parser
import qualified Snap


main = do
    mapM mkdir
      [ "src/Api"
      , "log"
      , "image"
      ]

    apiFiles <- Parser.findApiFiles
    modules <- mapM Parser.parseApiFile apiFiles

    mapM_ CodeGen.makeElmUserType modules
    CodeGen.makeElmCmsType modules

    imageFiles <- Image.findImageFiles

    let
        apiRoutes =
            List.map apiRoute modules

    Snap.httpServe Snap.defaultConfig $ do
      Snap.route
        ( [
            [ (,) "" $ do
                Snap.writeText =<< liftIO (String.readFile "build/index.html")
            , (,) "elm.js" $ do
                Snap.writeText =<< liftIO (String.readFile "build/elm.js")
            ]

          , Image.imageRoute

          , List.concat apiRoutes
          ]
          & List.concat
        )


apiRoute :: Module -> List (LB.ByteString, Snap.Snap ())
apiRoute (Module (ApiDecl kind type_ idField) types _) =
    let
        endpoint =
            toLowercase type_

        typeDecl =
            case Map.lookup type_ types of
                Just typeDecl ->
                    typeDecl
                Nothing ->
                    error "typeDecl"
    in
    [ -- action list:
      (,) (LB.pack (String.unpack endpoint)) $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
            files <- liftIO $ findFiles (String.unpack endpoint)
            let
                jsonFiles =
                    files
                    & List.filter ( isSuffixOf ".json" )
            parsedJsonFiles <-
                liftIO $ forM jsonFiles ( \ jsonFile ->
                    fmap Aeson.decode (ByteString.readFile jsonFile) )
            Snap.writeLBS $
                Aeson.encode (List.map (verify typeDecl) (List.filterMap identity parsedJsonFiles))

    , -- action get:
      (,) (LB.pack (String.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
          id_ <- fmap (Maybe.withDefault "" . Maybe.map LB.unpack) (Snap.getParam "id")
          let
              fn =
                  String.unpack endpoint Directory.</> (id_ ++ ".json")
          doesFileExist <- liftIO $ Directory.doesFileExist fn
          if doesFileExist then do
              file <- fmap Aeson.decode (liftIO (ByteString.readFile fn))
              case file of  
                  Just file ->
                      case verify typeDecl file of
                          Just file ->
                              Snap.writeLBS (Aeson.encode file)
                          Nothing ->
                              Snap.writeText "does not verify"
                  Nothing ->
                      Snap.writeText "does not parse"
          else do
              Snap.writeText "does not exist"

    , -- action update:
      (,) (LB.pack (String.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.POST $  do
          id_ <- fmap (Maybe.withDefault "" . Maybe.map LB.unpack) (Snap.getParam "id")
          obj <- fmap Aeson.decode (Snap.readRequestBody 1048576)
          case obj of
              Just obj ->
                  case verify typeDecl obj of
                      Just obj -> do
                          let
                              fn =
                                  String.unpack endpoint Directory.</> (id_ ++ ".json")
                          if Directory.normalize fn == fn then do
                              liftIO $ ByteString.writeFile fn (Aeson.encode obj)
                              Snap.writeLBS (Aeson.encode obj)
                          else
                              Snap.writeText "does not normalize"
                      Nothing ->
                          Snap.writeText "does not verify"
              Nothing ->
                  Snap.writeText "does not prase"

    , -- action delete
      (,) (LB.pack (String.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.DELETE $  do
          id_ <- fmap (Maybe.withDefault "" . Maybe.map LB.unpack) (Snap.getParam "id")
          let
              fn =
                  String.unpack endpoint Directory.</> (id_ ++ ".json")
          if Directory.normalize fn == fn then do
              doesExist <- liftIO (Directory.doesFileExist fn)
              if doesExist then do
                  storedObj <- fmap Aeson.decode (liftIO (ByteString.readFile fn))
                  case storedObj of
                      Just storedObj -> do
                          case verify typeDecl storedObj of
                              Just parsedStoredObj -> do
                                  liftIO $ Directory.removeFile fn
                                  Snap.writeLBS (Aeson.encode parsedStoredObj)
                              Nothing -> do
                                  liftIO $ Directory.removeFile fn
                                  Snap.writeText "does not verify"
                      Nothing ->
                          Snap.writeText "does not parse"
              else
                  Snap.writeText "does not exist"
          else
              Snap.writeText "does not normalize"

    , -- action create:
      (,) (LB.pack (String.unpack endpoint)) $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.POST $  do
          obj <- fmap Aeson.decode (Snap.readRequestBody 4096)
          case obj of
              Just obj ->
                  case verify typeDecl obj of
                      Just obj -> do
                          let
                              fn =
                                  String.unpack endpoint Directory.</> (id_ ++ ".json")
                              id_ =
                                  case HashMap.lookup idField obj of
                                      Just (Aeson.String id_) -> String.unpack id_
                                      _ -> ""
                          if Directory.normalize fn == fn then do
                              liftIO $ ByteString.writeFile fn (Aeson.encode obj)
                              Snap.writeLBS (Aeson.encode obj)
                          else
                              Snap.writeText "does not normalize"
                      Nothing ->
                          Snap.writeText "does not verify"
              Nothing ->
                  Snap.writeText "does not prase"
    ]


verify (TypeDecl typeName fields) value =
    case value of
        Aeson.Object obj ->
            let
                typechecks =
                    fields
                    & List.map (\ ( fieldName, typeRep ) ->
                        let
                            value =
                                HashMap.lookup fieldName obj
                                & Maybe.withDefault (defaultValue typeRep)
                        in
                          unifies typeRep value
                      )
                    & List.foldl (&&) True

                defaultValue :: TypeRep -> Aeson.Value
                defaultValue typeRep =
                    case typeRep of
                        TString ->
                            Aeson.String ""
                        TBool ->
                            Aeson.Bool False
                        TInt ->
                            Aeson.Number 0
                        TFloat ->
                            Aeson.Number 0
                        TMaybe _ ->
                            Aeson.Null
                        TList _ ->
                            Aeson.Array (Vector.empty)

                unifies typeRep value =
                    case (typeRep, value) of
                        (TString, Aeson.String _) ->
                            True

                        (TBool, Aeson.Bool _) ->
                            True

                        (TInt, Aeson.Number _) ->
                            True -- TODO

                        (TFloat, Aeson.Number _) ->
                            True

                        (TMaybe _, Aeson.Null) ->
                            True

                        (TMaybe typeRep, _) ->
                            unifies typeRep value

                        (TList typeRep, Aeson.Array v) ->
                            v
                            & Vector.map (unifies typeRep)
                            & Vector.foldl (&&) True

                        _ ->
                            False
            in
            if typechecks then
                Just obj
            else
                Nothing
        _ ->
            Nothing
