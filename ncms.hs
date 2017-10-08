module Main where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import Control.Exception (catch)
import Control.Monad (join, sequence, unless, when)
import Control.Monad (Monad(..), mapM, mapM_, forM, forM_, (=<<))
import Control.Monad.Trans (liftIO)
import Core
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Either (Either(Left,Right))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Prelude (Show, Bool(..))
import qualified Core.List as List
import qualified Core.String as String
import qualified Core.Tuple as Tuple
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Parser
import qualified Data.ByteString.Char8 as LB
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Snap
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Process as Process


main = do
    apiFiles <-
        liftIO (findFiles ".")
        & fmap (List.map (\ fp -> List.dropLeft 2 fp))
        & fmap (
            List.filter ( \ fp ->
              [ isPrefixOf "_"
              , not . isSuffixOf "~"
              ]
              & List.map (\ f -> f  fp )
              & and
            )
          )

    modules <-
        forM apiFiles $ \ apiFile -> do
            parsedApiFile <- fmap parse (readFile apiFile)
            case parsedApiFile of
                Right parsedApiFile ->
                    return parsedApiFile
                Left e ->
                    error (String.unpack e)

    mapM mkdir
        [ "src/Api"
        , "log"
        , "image"
        ]

    forM_ modules $ \ mod_@(Module (ApiDecl kind type_ idField) types _) -> do
        let
            moduleName =
                "Api." `String.append` type_

            fp =
                "src/Api/" `String.append` type_ `String.append` ".elm"
                & String.unpack

        writeFile fp (elmApi moduleName mod_)

    writeFile "src/Api.elm" (elmApis modules)

    let
        apiRoutes =
            List.map apiRoute modules

    imageFiles <- do
        imageFiles <-
            findFiles "image"
            & fmap (List.filter (not . isSuffixOf ".json"))
            & liftIO
        forM_ imageFiles $ \ imageFile -> do
            let
                jsonFn =
                    imageFile
                    & FilePath.dropExtension
                    & flip (++) ".json"
            doesExist <- Directory.doesFileExist jsonFn
            unless doesExist $ do
                liftIO $ IO.tryIOError (createImageFileMeta imageFile jsonFn)
                return ()

    Snap.httpServe Snap.defaultConfig $ do
      Snap.route
        ( [
            [ (,) "" $ do
                Snap.writeText =<< liftIO (readFile "build/index.html")
            , (,) "elm.js" $ do
                Snap.writeText =<< liftIO (readFile "build/elm.js")
            ]

          , imageRoute

          , List.concat apiRoutes
          ]
          & List.concat
        )


createImageFileMeta imageFile jsonFile = do
    let
        identify fn = do
            fmap String.pack (Process.readCreateProcess proc "")
          where
            proc =
                Process.shell
                ( List.concat
                  [ "identify -format '{"
                  , "\"size\": \"%b\","
                  , "\"directory\": \"%d\","
                  , "\"extension\": \"%e\","
                  , "\"file\": \"%t\","
                  , "\"width\": %w,"
                  , "\"height\": %h"
                  , "}' '" ++ fn ++ "'"
                  ]
                )
    writeFile jsonFile =<< identify imageFile


imageRoute :: List (LB.ByteString, Snap.Snap ())
imageRoute =
    [ -- action read:
      (,) "image/:id" $ do
        Snap.method Snap.GET $  do
          accept <-
              Snap.getRequest
              & fmap (Snap.getHeader "Accept")
              & fmap (Maybe.maybe "*/*" identity)

          unless ( or
                   [ LB.isInfixOf "*/*" accept
                   , LB.isInfixOf "image/*" accept
                   , LB.isInfixOf "image/png" accept
                   ]
                 ) $
              Snap.pass

          id_ <-
              Snap.getParam "id"
              & fmap (Maybe.maybe "" identity)
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((FilePath.</>) ("image" FilePath.</> (id_)))
          Snap.modifyResponse $
              Snap.setContentType "image/png"
          doesExist <- liftIO $ Directory.doesFileExist fn
          if doesExist then
              Snap.writeLBS =<< liftIO (ByteString.readFile fn)
          else
              Snap.modifyResponse $ Snap.setResponseStatus 404 "Not Found"

    , -- action update:
      (,) "image/:id" $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.POST $  do
          id_ <-
              Snap.getParam "id"
              & fmap (Maybe.maybe "" identity)
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((FilePath.</>) ("image" FilePath.</> (id_)))
          body <- Snap.readRequestBody 104857600
          fileExists <- liftIO $ Directory.doesFileExist fn
          if fileExists then
              Snap.writeText "does exist"
          else do
              let
                  jsonFn =
                      fn
                      & FilePath.dropExtension
                      & flip (++) ".json"
              liftIO $ do
                  ByteString.writeFile fn body
                  IO.tryIOError (createImageFileMeta fn jsonFn)
              Snap.writeLBS (Aeson.encode fn)

    , -- action get:
      (,) "image/:id" $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
          id_ <-
              Snap.getParam "id"
              & fmap (Maybe.maybe "" identity)
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((FilePath.</>) ("image" FilePath.</> (id_)))
          fileExists <- liftIO $ Directory.doesFileExist fn
          if fileExists then do
              Snap.writeLBS =<< liftIO (ByteString.readFile fn)
          else do
              Snap.modifyResponse $ Snap.setResponseStatus 404 "Not Found"

    , -- action delete
      (,) "image/:id" $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        id_ <-
            Snap.getParam "id"
            & fmap (Maybe.maybe "" identity)
            & fmap LB.unpack
        fn <-
            Snap.getsRequest Snap.rqPathInfo
            & fmap LB.unpack
            & fmap ((FilePath.</>) ("image" FilePath.</> (id_)))
        let
            jsonFn =
                fn
                & FilePath.dropExtension
                & flip (++) ".json"
        doesExist <- liftIO $ Directory.doesFileExist fn
        doesJsonExist <- liftIO $ Directory.doesFileExist jsonFn
        when doesExist $ do
            liftIO $ Directory.removeFile fn
        when doesJsonExist $ do
            liftIO $ Directory.removeFile jsonFn

    , -- action list:
      (,) "image" $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
            files <- liftIO $ findFiles "image"
            let
                jsonFiles =
                    files
                    & List.filter ( isSuffixOf ".json" )
            jsonFileContents <-
                forM jsonFiles $ \ jsonFile -> do
                    fileContents <- liftIO $ ByteString.readFile jsonFile
                    return (Aeson.decode fileContents :: Maybe Aeson.Value)
            Snap.writeLBS $
                Aeson.encode (Maybe.catMaybes jsonFileContents)

--    , -- action create:
--      (,) "image" $ do
--        Snap.modifyResponse $
--            Snap.setContentType "application/json"
--        Snap.writeText "not implemented"
    ]


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
                Aeson.encode (List.map (verify typeDecl) (Maybe.catMaybes parsedJsonFiles))

    , -- action get:
      (,) (LB.pack (String.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
          id_ <- fmap (Maybe.maybe "" LB.unpack) (Snap.getParam "id")
          let
              fn =
                  String.unpack endpoint FilePath.</> (id_ ++ ".json")
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
          id_ <- fmap (Maybe.maybe "" LB.unpack) (Snap.getParam "id")
          obj <- fmap Aeson.decode (Snap.readRequestBody 1048576)
          case obj of
              Just obj ->
                  case verify typeDecl obj of
                      Just obj -> do
                          let
                              fn =
                                  String.unpack endpoint FilePath.</> (id_ ++ ".json")
                          if FilePath.normalise fn == fn then do
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
          id_ <- fmap (Maybe.maybe "" LB.unpack) (Snap.getParam "id")
          let
              fn =
                  String.unpack endpoint FilePath.</> (id_ ++ ".json")
          if FilePath.normalise fn == fn then do
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
                                  String.unpack endpoint FilePath.</> (id_ ++ ".json")
                              id_ =
                                  case HashMap.lookup idField obj of
                                      Just (Aeson.String id_) -> String.unpack id_
                                      _ -> ""
                          if FilePath.normalise fn == fn then do
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
                                & Maybe.maybe (defaultValue typeRep) identity
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


findFiles :: FilePath -> IO (List FilePath)

findFiles "" =
    findFiles "."
    & fmap (List.map (List.dropLeft 2))

findFiles dir = do
    dirs <- findDirectories dir
    fmap (fmap List.concat) $ forM (dir:dirs) $ \dir ->
        Directory.listDirectory dir
        & fmap (
            List.map ( \ fn -> do
              fileExists <- Directory.doesFileExist (dir FilePath.</> fn)
              if fileExists then
                  return (Just (dir FilePath.</> fn))
              else
                  return Nothing
            )
          )
        & join . fmap sequence
        & fmap Maybe.catMaybes

findDirectories :: FilePath -> IO (List FilePath)
findDirectories =
    findWith Directory.doesDirectoryExist

findWith :: (FilePath -> IO Bool) -> FilePath -> IO (List FilePath)

findWith pred "" = do
    findWith pred "."

findWith pred dir = do
    let
        find_ dir =
            Directory.listDirectory dir
            & fmap (
                List.map ( \ fn -> do
                  isTrue <- pred (dir FilePath.</> fn)
                  if isTrue then
                      return (Just (dir FilePath.</> fn))
                  else
                      return Nothing
                )
              )
            & join . fmap sequence
            & fmap Maybe.catMaybes
    dirs <- find_ dir
    (++) dirs <$> (fmap List.concat $ mapM find_ dirs)


elmApis modules =
    let
        (+) =
            String.append

        showTypeRep typeRep =
            case typeRep of
                TString -> "String"
                TBool -> "Bool"
                TInt -> "Int"
                TFloat -> "Float"
                TMaybe typeRep -> "Maybe (" + showTypeRep typeRep + ")"
                TList typeRep -> "List (" + showTypeRep typeRep + ")"
    in
    String.join "\n"
    [ "module Api exposing ( apis )"
    , "\n"
    , "import Http exposing (Error)"
    , "import Json.Decode as Json exposing (Value)"
    , "import Ncms.Backend exposing (Rest,Prim(..))"
    , "import Ncms.Backend.Ncms as Backend"
    , "import Task exposing (Task)"
    , modules
      & List.map (\ (Module (ApiDecl kind type_ idField) types _) ->
          "import Api." + type_ + " as " + type_
        )
      & String.join "\n"
    , "\n"
    , "-- API"
    , "\n"
    , modules
      & List.map (\ (Module (ApiDecl kind type_ idField) types _) ->
          let
              endpoint =
                  toLowercase type_

              mainType =
                Map.elems types
                & List.filter (\ (TypeDecl typeName fields) ->
                      typeName == type_
                  )
                & head
                & Maybe.maybe (error "no main type") identity

              otherTypes =
                  Map.elems types
                  & List.filter (\ (TypeDecl typeName fields) ->
                        typeName /= type_
                    )

              makeType (TypeDecl typeName fields) =
                  let
                      ( idField : otherFields ) =
                          fields

                      makeField ( fieldName, typeRep ) =
                          [ "name = \"" + fieldName + "\""
                          , "tipe = " + makePrim typeRep
                          ]
                          & String.join "\n              , "
                          & String.append "              { "
                          & flip String.append "\n              }"

                      makePrim typeRep =
                          case typeRep of
                              TString ->
                                  "String"
                              TBool ->
                                  "Bool"
                              TInt ->
                                  "Int"
                              TFloat ->
                                  "Float"
                              TMaybe typeRep' ->
                                  "Maybe (" + makePrim typeRep' + ")"
                              TList typeRep' ->
                                  "List (" + makePrim typeRep' + ")"
                  in
                  [ "name = \"" + typeName + "\""
                  , "idField = \n" + makeField idField
                  , otherFields
                    & List.map makeField
                    & String.join "\n        ,"
                    & String.append "fields =\n              [\n"
                    & flip String.append "\n              ]"
                  ]
                  & String.join "\n          , "
                  & String.append "          { "
                  & flip String.append "\n          }"
        in
        [
          "tipe =\n" + makeType mainType

        , ( case kind of
                "ncms" ->
                    [ "get = Backend.get \"" + endpoint + "\" identity Json.value"
                    , "update = Backend.update \"" + endpoint + "\" identity Json.value (Backend.idField \"" + idField + "\")"
                    , "delete = Backend.delete \"" + endpoint + "\" identity Json.value"
                    , "create = Backend.create \"" + endpoint + "\" identity Json.value (Backend.idField \"" + idField + "\")"
                    , "list = Backend.list \"" + endpoint + "\" identity Json.value"
                    ]
                "github" ->
                    [ "get = Github.get \"" + endpoint + "\" identity Json.value"
                    , "update = Github.update \"" + endpoint + "\" identity Json.value (Github.idField \"" + idField + "\")"
                    , "delete = Github.delete \"" + endpoint + "\" identity Json.value"
                    , "create = Github.create \"" + endpoint + "\" identity Json.value (Github.idField \"" + idField + "\")"
                    , "list = Github.list \"" + endpoint + "\" identity Json.value"
                    ]
                _ ->
                    error "unsupported type"
          )
          & String.join "\n    , "
        ]
        & String.join "\n    , "
        & String.append "\n    { "
        & flip String.append "\n    }"
      )
    & String.join "\n  , "
    & String.append "apis =\n  [ "
    & String.append "apis : List (Rest Value)\n"
    & flip String.append "\n  ] "
  ]


elmApi modName (Module (ApiDecl kind type_ idField) types _) =
  let
      endpoint =
          toLowercase type_

      exposing =
          List.concat
          [ [ "get"
            , "update"
            , "delete"
            , "create"
            , "list"
            ]
          , Map.keys types
          , Map.keys types
            & List.map (\ typeName -> "default" + typeName)
          , Map.keys types
            & List.map (\ typeName -> "encode" + typeName)
          , Map.keys types
            & List.map (\ typeName -> toLowercase typeName + "Decoder")
          ]

      (+) =
          String.append

      indexTypeRep =
          types
          & Map.lookup type_
          & Maybe.maybe (error "indexTypeRep")
            ( \ type_ ->
              case type_ of
                  ( TypeDecl _ fields ) ->
                      fields
                      & List.filter ( (==) idField . Tuple.first )
                      & ( \xs ->
                            case xs of
                                [ ( _, typeRep ) ] -> typeRep
                                _ -> error "indexTypeRep"
                        )
            )

      get =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "get : " + showTypeRep indexTypeRep + " -> Task Error " + type_
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "get : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      delete =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "delete : " + showTypeRep indexTypeRep + " -> Task Error ()"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "delete : (Result Error () -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      update =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "update : " + type_ + " -> Task Error ()"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  String.join "\n"
                  [ "update : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      create =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "create : " + type_ + " -> Task Error ()"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  String.join "\n"
                  [ "create : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      list =
          case kind of
              "ncms" ->
                  String.join "\n"
                  [ "list : Task Error (List " + type_ + ")"
                  , "list ="
                  , "    Backend.list \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  String.join "\n"
                  [ "list : (Result Error (List " + type_ + ") -> msg) -> String -> String -> String -> Cmd msg"
                  , "list ="
                  , "    Backend.list \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      api =
          [ get
          , update
          , delete
          , list
          , create
          ]
          & String.join "\n\n"
          & flip String.append "\n\n"

      encoders =
          let

              encodeTypeRep typeRep =
                  case typeRep of
                      TString -> "Encode.string"
                      TBool -> "Encode.bool"
                      TInt -> "Encode.int"
                      TFloat -> "Encode.float"
                      TMaybe typeRep ->
                          "(Maybe.withDefault Encode.null << Maybe.map ("
                          + encodeTypeRep typeRep + "))"
                      TList typeRep ->
                          "Encode.list << List.map " + encodeTypeRep typeRep + "<|"

              decodeTypeRep (TypeDecl typeName fields)  =
                  String.join "\n"
                  [ "encode" + typeName + " : " + typeName + " -> Decode.Value"
                  , "encode" + typeName + " value ="
                  , fields
                    & List.map (\ ( fieldName, typeRep ) ->
                        "( \"" + fieldName + "\", "
                        + encodeTypeRep typeRep  + " value." + fieldName + " )"
                      )
                    & String.join "\n    , "
                    & String.append "    [ "
                    & flip String.append "\n    ]"
                  , "    |> Encode.object"
                  ]
          in
          Map.elems types
          & List.map decodeTypeRep
          & String.join "\n\n"
          & flip String.append "\n\n"

      decoders =
          let
              typeRepDecoder typeRep =
                  case typeRep of
                      TString -> "Decode.string"
                      TBool -> "Decode.bool"
                      TInt -> "Decode.int"
                      TFloat -> "Decode.float"
                      TMaybe typeRep -> "(Decode.maybe " + typeRepDecoder typeRep + ")"
                      TList typeRep -> "(Decode.list " + typeRepDecoder typeRep + ")"


              decodeTypeRep (TypeDecl typeName fields)  =
                  String.join "\n"
                  [ toLowercase typeName + "Decoder : Decoder " + typeName
                  , toLowercase typeName + "Decoder ="
                  , fields
                    & List.map (\ ( fieldName, typeRep ) ->
                        "        ( Decode.at [ \"" + fieldName + "\" ] "
                        + typeRepDecoder typeRep  + " )"
                      )
                    & String.join "\n"
                    & String.append (
                          if List.length fields == 1 then
                              "    Decode.map " + typeName + "\n"
                          else
                              "    Decode.map" + String.pack (show (List.length fields)) + " " + typeName + "\n"
                      )
                  ]
          in
          Map.elems types
          & List.map decodeTypeRep
          & String.join "\n\n"
          & flip String.append "\n\n"

      showTypeRep typeRep =
          case typeRep of
              TString -> "String"
              TBool -> "Bool"
              TInt -> "Int"
              TFloat -> "Float"
              TMaybe typeRep -> "Maybe (" + showTypeRep typeRep + ")"
              TList typeRep -> "List (" + showTypeRep typeRep + ")"

      typeDefs =
          Map.elems types
          & List.map ( \ (TypeDecl typeName fields) ->
                String.join "\n"
                [ "type alias "  + typeName + " ="
                , fields
                  & List.map (\ (fieldName, typeRep) ->
                        fieldName + " : " + showTypeRep typeRep
                    )
                  & String.join "\n    , "
                  & String.append "    { "
                  & flip String.append "\n    }"
                ]
            )
          & String.join "\n\n"

      typeRepDefault typeRep =
          case typeRep of
              TString -> "\"\""
              TBool -> "False"
              TInt -> "-1"
              TFloat -> "0.0"
              TMaybe typeRep -> "Nothing"
              TList typeRep -> "[]"

      defaultTypes =
          Map.elems types
          & List.map ( \ (TypeDecl typeName fields) ->
                String.join "\n"
                [ "default" + typeName + " : " + typeName
                , "default" + typeName + " ="
                , fields
                  & List.map (\ (fieldName, typeRep) ->
                        fieldName + " = " + typeRepDefault typeRep
                    )
                  & String.join "\n    , "
                  & String.append "    { "
                  & flip String.append "\n    }"
                ]
            )
          & String.join "\n\n"
  in
  String.join "\n"
  [ "module " + modName + " exposing"
  , "    ( "
  ,
    exposing
    & String.join "\n    , "
    & String.append "      "

  , "    )"
  , "\n"
  , "import Http exposing (Error)"
  , "import Task exposing (Task)"
  , "import Json.Decode as Decode exposing (Decoder)"
  , "import Json.Encode as Encode"
  , case kind of
      "ncms" ->
        "import Ncms.Backend.Ncms as Backend"
      "github" ->
        "import Ncms.Backend.Github as Backend"
      _ ->
        error "unsupported kind"
  , "\n"

  , "-- API"
  , "\n"
  , api

  , "-- TYPES"
  , "\n"
  , typeDefs
  , "\n"
  , defaultTypes
  , "\n"

  , "-- DECODER"
  , "\n"
  , decoders

  , "-- ENCODER"
  , "\n"
  , encoders
  ]


readFile :: FilePath -> IO String
readFile =
  String.readFile


writeFile :: FilePath -> String -> IO ()
writeFile fileName contents =
  String.writeFile fileName contents


data Module =
  Module ApiDecl (Map String TypeDecl) String deriving Show


fromExprs :: List Expr -> Either String Module
fromExprs exprs =
  let
      (apiDecls, typeDecls, garbage) =
          List.foldl
            (\(apiDecls, typeDecls, garbage') expr ->
                case expr of
                    ETypeDecl typeDecl ->
                        ( apiDecls, typeDecl:typeDecls, garbage' )
                    EApiDecl apiDecl ->
                        ( apiDecl:apiDecls, typeDecls, garbage' )
                    EEof garbage ->
                        ( apiDecls, typeDecls,
                              garbage
                              & String.append garbage'
                        )
            )
            ([], [], "")
            exprs
  in
      case (apiDecls, typeDecls) of
          ([apiDecl], typeDecls) ->
              Right $
              Module
                  apiDecl
                  ( typeDecls
                    & List.map (\ typeDecl ->
                        case typeDecl of
                            TypeDecl typeName _ -> (typeName, typeDecl)
                      )
                    & Map.fromList
                  )
                  garbage

          (_, _) ->
              Left $ "more than one api decl"


parse :: String -> Either String Module
parse input =
    fromExprs $
    case Parser.parseOnly exprs input of
        Left parserError ->
            error "parserError"
        Right exprs ->
            exprs


data Expr
  = EApiDecl ApiDecl
  | ETypeDecl TypeDecl
  | EEof String
  deriving Show


exprs :: Parser (List Expr)
exprs =
  let
      token =
          (fmap (Just . EApiDecl) apiDecl)
            <|>
            (fmap (Just . ETypeDecl) typeDecl)
                <|> pure Nothing
  in
      token >>= \token ->
      case token of
          Just token ->
              (:) <$> (pure token) <*> exprs
          Nothing ->
              fmap (List.singleton . EEof) (Parser.takeText)


data ApiDecl
  = ApiDecl String String String
  deriving Show


data TypeDecl
  = TypeDecl String (List (String, TypeRep))
  deriving Show


apiDecl :: Parser ApiDecl
apiDecl =
  ( oneOf
    [ ApiDecl "ncms"
        <$> (string "rest_api of type " *> typeName <* string "indexed on ")
        <*> (word <* string ":")
    , ApiDecl "github"
        <$> (string "github_api of type " *> typeName <* string "indexed on ")
        <*> (word <* string ":")
    ]
  ) & lexeme


typeName :: Parser String
typeName =
  word


word :: Parser String
word =
  many Parser.letter
  & fmap String.pack
  & lexeme


typeDecl :: Parser TypeDecl
typeDecl =
  TypeDecl
      <$> (string "type" *> typeName)
      <*> ( do Parser.char '='
               many space
               lexeme (many field)
          )


field :: Parser (String, TypeRep)
field = do
  result <- (,) <$> (string "\n " *> word) <*> (string ":" *> typeRep)
  many space
  pure result


data TypeRep
  = TString
  | TBool
  | TInt
  | TFloat
  | TMaybe TypeRep
  | TList TypeRep
  deriving Show


typeRep :: Parser TypeRep
typeRep =
  oneOf
  [ fmap (const TString) (symbol "String")
  , fmap (const TBool) (symbol "Bool")
  , fmap (const TInt) (symbol "Int")
  , fmap (const TFloat) (symbol "Float")
  , string "Maybe (" *>
      ( ( string "Maybe"
          *> fail "Maybe (Maybe …) is not a type"
        )
          <|> ( fmap TMaybe (string "Maybe (" *> typeRep <* string ")") )
      )
  , fmap TMaybe (string "Maybe " *> typeRep)
  , fmap TList (string "List " *> typeRep)
  ]


string :: String -> Parser String
string str =
  symbol str
  & lexeme


symbol :: String -> Parser String
symbol str =
  Parser.string str


lexeme :: Parser a -> Parser a
lexeme parser =
  parser <* many (fmap void space <|> Parser.endOfLine)


space :: Parser ()
space =
  oneOf [ Parser.char ' ', Parser.char '\t' ]
  & fmap void


many :: Parser a -> Parser (List a)
many =
  Parser.many'


oneOf :: List (Parser a) -> Parser a
oneOf =
  Parser.choice
