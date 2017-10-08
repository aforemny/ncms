module Image
  ( findImageFiles
  , imageRoute
  )
  where

import Core
import qualified Core.Directory as Directory
import qualified Core.List as List
import qualified Core.Maybe as Maybe
import qualified Core.String as String
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as LB
import qualified Data.ByteString.Lazy as ByteString
import qualified Snap
import qualified System.Process as Process
import System.IO.Error as IO


findImageFiles = do
  imageFiles <- do
      Directory.findFiles "image"
      & fmap (List.filter (not . isSuffixOf ".json"))
      & liftIO
  forM_ imageFiles $ \ imageFile -> do
      let
          jsonFn =
              imageFile
              & Directory.dropExtension
              & flip (++) ".json"
      doesExist <- Directory.doesFileExist jsonFn
      unless doesExist $ do
          liftIO $ IO.tryIOError (createImageFileMeta imageFile jsonFn)
          return ()
  return imageFiles


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
    String.writeFile jsonFile =<< identify imageFile


imageRoute :: List (LB.ByteString, Snap.Snap ())
imageRoute =
    [ -- action read:
      (,) "image/:id" $ do
        Snap.method Snap.GET $  do
          accept <-
              Snap.getRequest
              & fmap (Snap.getHeader "Accept")
              & fmap (Maybe.withDefault "*/*")

          unless ( or
                   [ LB.isInfixOf "*/*" accept
                   , LB.isInfixOf "image/*" accept
                   , LB.isInfixOf "image/png" accept
                   ]
                 ) $
              Snap.pass

          id_ <-
              Snap.getParam "id"
              & fmap (Maybe.withDefault "")
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((Directory.</>) ("image" Directory.</> (id_)))
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
              & fmap (Maybe.withDefault "")
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((Directory.</>) ("image" Directory.</> (id_)))
          body <- Snap.readRequestBody 104857600
          fileExists <- liftIO $ Directory.doesFileExist fn
          if fileExists then
              Snap.writeText "does exist"
          else do
              let
                  jsonFn =
                      fn
                      & Directory.dropExtension
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
              & fmap (Maybe.withDefault "")
              & fmap LB.unpack
          fn <-
              Snap.getsRequest Snap.rqPathInfo
              & fmap LB.unpack
              & fmap ((Directory.</>) ("image" Directory.</> (id_)))
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
            & fmap (Maybe.withDefault "")
            & fmap LB.unpack
        fn <-
            Snap.getsRequest Snap.rqPathInfo
            & fmap LB.unpack
            & fmap ((Directory.</>) ("image" Directory.</> (id_)))
        let
            jsonFn =
                fn
                & Directory.dropExtension
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
            files <- liftIO $ Directory.findFiles "image"
            let
                jsonFiles =
                    files
                    & List.filter ( isSuffixOf ".json" )
            jsonFileContents <-
                forM jsonFiles $ \ jsonFile -> do
                    fileContents <- liftIO $ ByteString.readFile jsonFile
                    return (Aeson.decode fileContents :: Maybe Aeson.Value)
            Snap.writeLBS $
                Aeson.encode (List.filterMap identity jsonFileContents)

--    , -- action create:
--      (,) "image" $ do
--        Snap.modifyResponse $
--            Snap.setContentType "application/json"
--        Snap.writeText "not implemented"
    ]
