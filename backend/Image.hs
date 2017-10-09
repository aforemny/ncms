module Image
  ( findImageFiles
  , imageRoute
  )
  where

import Core
import qualified Config
import qualified Core.Directory as Directory
import qualified Core.List as List
import qualified Core.Maybe as Maybe
import qualified Core.Process as Process
import qualified Core.String as String
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as LB
import qualified Data.ByteString.Lazy as ByteString
import qualified Snap
import System.IO.Error as IO


findImageFiles config = do
  imageFiles <- do
      Directory.findFiles (Config.imageDirectory config)
      & fmap (List.filter (not . isSuffixOf ".json"))
      & liftIO
  forM_ imageFiles $ \ imageFile -> do
      let
          file =
              imageFile
              & Directory.makeRelative (Config.imageDirectory config)

          jsonFn =
              imageFile
              & Directory.dropExtension
              & flip (++) ".json"
      doesExist <- Directory.doesFileExist jsonFn
      unless doesExist $ do
          liftIO $ IO.tryIOError (createImageFileMeta file imageFile jsonFn)
          return ()
  return imageFiles


createImageFileMeta :: FilePath -> FilePath -> FilePath -> IO ()
createImageFileMeta file imageFile jsonFile = do
    let
        directory =
            Directory.takeDirectory file

        baseName =
            Directory.takeBaseName file

    String.writeFile jsonFile
        =<< identify
              ( List.map String.pack
                [ ( List.concat
                    [ "-format '{"
                    , "\"size\": \"%b\","
                    , "\"directory\": \"" ++ directory ++ "\","
                    , "\"extension\": \"%e\","
                    , "\"file\": \"" ++ baseName ++ "\","
                    , "\"width\": %w,"
                    , "\"height\": %h"
                    , "}'"
                    ]
                  )
                , "'" ++ imageFile ++ "'"
                ]
              )
              ""
    return ()


identify =
    Process.shell "identify"


imageRoute config =
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
              & fmap ((Directory.</>) (Config.imageDirectory config Directory.</> (id_)))
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
              & fmap ((Directory.</>) (Config.imageDirectory config Directory.</> (id_)))
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
                  file =
                      fn
                      & Directory.makeRelative (Config.imageDirectory config)
              liftIO $ do
                  ByteString.writeFile fn body
                  IO.tryIOError (createImageFileMeta file fn jsonFn)
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
              & fmap ((Directory.</>) (Config.imageDirectory config Directory.</> (id_)))
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
            & fmap ((Directory.</>) (Config.imageDirectory config Directory.</> (id_)))
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
            files <- liftIO $ Directory.findFiles $
                Directory.normalize $
                Config.imageDirectory config
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
