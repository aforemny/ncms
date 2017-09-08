{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import Control.Monad (join, sequence)
import Control.Monad (Monad(..), mapM, mapM_, forM, forM_, (=<<))
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Either (Either(Left,Right))
import Data.Function ((&))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Prelude (Show(show), Bool(..), IO, concat)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Parser
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as LB
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Prelude
import qualified Snap
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO.Unsafe


main = do
    let
        debug =
            Snap.writeText . Text.pack . show

    apiFiles <-
        liftIO (findFiles ".")
        & fmap (map (\ fp -> Prelude.drop 2 fp))
        & fmap (
            Prelude.filter ( \ fp ->
              [ isPrefixOf "_"
              , Prelude.not . isSuffixOf "~"
              ]
              & map (\ f -> f  fp )
              & Prelude.and
            )
          )

    modules <-
        forM apiFiles $ \ apiFile -> do
            parsedApiFile <- fmap parse (readFile apiFile)
            case parsedApiFile of
                Right parsedApiFile ->
                    return parsedApiFile
                Left e ->
                    error (Text.unpack e)

    mkdir "src/Api"
    forM_ modules $ \ mod_@(Module (ApiDecl kind type_ idField) types _) -> do
        let
            moduleName =
                "Api." `Text.append` type_

            fp =
                "src/Api/" `Text.append` type_ `Text.append` ".elm"
                & Text.unpack

        writeFile fp (elmApi moduleName mod_)

    writeFile "src/Api.elm" (elmApis modules)

    let
        _users = error "_users"

    let
        apiRoutes =
            map apiRoute modules

    mkdir "log"
    Snap.httpServe Snap.defaultConfig $ do
      Snap.route
        ( [
            [ (,) "" $ do
                Snap.writeText =<< liftIO (readFile "build/index.html")
            , (,) "elm.js" $ do
                Snap.writeText =<< liftIO (readFile "build/elm.js")
            ]

          , concat apiRoutes
          ]
          & concat
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
      (,) (LB.pack (Text.unpack endpoint)) $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
            files <- liftIO $ findFiles (Text.unpack endpoint)
            let
                jsonFiles =
                    files
                    & Prelude.filter ( isSuffixOf ".json" )
            parsedJsonFiles <-
                liftIO $ forM jsonFiles ( \ jsonFile ->
                    fmap Aeson.decode (ByteString.readFile jsonFile) )
            Snap.writeLBS $
                Aeson.encode (map (verify typeDecl) (Maybe.catMaybes parsedJsonFiles))

    , -- action get:
      (,) (LB.pack (Text.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.GET $  do
          id_ <- fmap (Maybe.maybe "" LB.unpack) (Snap.getParam "id")
          let
              fn =
                  Text.unpack endpoint FilePath.</> (id_ Prelude.++ ".json")
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
      (,) (LB.pack (Text.unpack endpoint) `LB.append` "/:id") $ do
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
                                  Text.unpack endpoint FilePath.</> (id_ Prelude.++ ".json")
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
      (,) (LB.pack (Text.unpack endpoint) `LB.append` "/:id") $ do
        Snap.modifyResponse $
            Snap.setContentType "application/json"
        Snap.method Snap.DELETE $  do
          id_ <- fmap (Maybe.maybe "" LB.unpack) (Snap.getParam "id")
          let
              fn =
                  Text.unpack endpoint FilePath.</> (id_ Prelude.++ ".json")
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
      (,) (LB.pack (Text.unpack endpoint)) $ do
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
                                  Text.unpack endpoint FilePath.</> (id_ Prelude.++ ".json")
                              id_ =
                                  case HashMap.lookup idField obj of
                                      Just (Aeson.String id_) -> Text.unpack id_
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
                    obj
                    & HashMap.mapWithKey typecheck
                    & HashMap.foldl' (Prelude.&&) True

                typecheck key value =
                    case Prelude.lookup key fields of
                        Just typeRep ->
                            unifies typeRep value
                        Nothing ->
                            False

                complete =
                    fields
                    & map (Prelude.flip HashMap.member obj . Prelude.fst)
                    & Prelude.foldl (Prelude.&&) True

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
                            & Vector.foldl (Prelude.&&) True

                        _ ->
                            False

            in
            if typechecks Prelude.&& complete then
                Just obj
            else
                Nothing
        _ ->
            Nothing


findFiles :: FilePath -> IO (List FilePath)

findFiles "" =
    findFiles "."
    & fmap (map (Prelude.drop 2))

findFiles dir = do
    dirs <- findDirectories dir
    fmap (fmap concat) $ forM (dir:dirs) $ \dir ->
        Directory.listDirectory dir
        & fmap (
            map ( \ fn -> do
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
                map ( \ fn -> do
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
    (Prelude.++) dirs <$> (fmap concat $ mapM find_ dirs)

isPrefixOf pre str =
    pre == Prelude.take (Prelude.length pre) str

isSuffixOf suf str =
    suf == Prelude.drop (Prelude.length str Prelude.- Prelude.length suf) str

mkdir =
    Directory.createDirectoryIfMissing True


elmApis modules =
    let
        (+) =
            Text.append

        showTypeRep typeRep =
            case typeRep of
                TString -> "String"
                TBool -> "Bool"
                TInt -> "Int"
                TFloat -> "Float"
                TMaybe typeRep -> "Maybe (" + showTypeRep typeRep + ")"
                TList typeRep -> "List (" + showTypeRep typeRep + ")"
    in
    Text.unlines
    [ "module Api exposing ( apis )"
    , "\n"
    , "import Http exposing (Error)"
    , "import Json.Decode as Json exposing (Value)"
    , "import Ncms.Backend exposing (Rest,Prim(..))"
    , "import Ncms.Backend.Ncms as Backend"
    , "import Task exposing (Task)"
    , modules
      & map (\ (Module (ApiDecl kind type_ idField) types _) ->
          "import Api." + type_ + " as " + type_
        )
      & Text.unlines
    , "\n"
    , "-- API"
    , "\n"
    , modules
      & map (\ (Module (ApiDecl kind type_ idField) types _) ->
          let
              endpoint =
                  toLowercase type_

              mainType =
                Map.elems types
                & Prelude.filter (\ (TypeDecl typeName fields) ->
                      typeName == type_
                  )
                & head
                & Maybe.maybe (error "no main type") Prelude.id

              otherTypes =
                  Map.elems types
                  & Prelude.filter (\ (TypeDecl typeName fields) ->
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
                          & Text.intercalate "\n              , "
                          & Text.append "              { "
                          & Prelude.flip Text.append "\n              }"

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
                              -- TMaybe TypeRep
                              -- TList TypeRep
                              _ ->
                                  "String"
                  in
                  [ "name = \"" + typeName + "\""
                  , "idField = \n" + makeField idField
                  , otherFields
                    & map makeField
                    & Text.intercalate "\n        ,"
                    & Text.append "fields =\n              [\n"
                    & Prelude.flip Text.append "\n              ]"
                  ]
                  & Text.intercalate "\n          , "
                  & Text.append "          { "
                  & Prelude.flip Text.append "\n          }"
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
          & Text.intercalate "\n    , "
        ]
        & Text.intercalate "\n    , "
        & Text.append "\n    { "
        & Prelude.flip Text.append "\n    }"
      )
    & Text.intercalate "\n  , "
    & Text.append "apis =\n  [ "
    & Text.append "apis : List (Rest Value)\n"
    & Prelude.flip Text.append "\n  ] "
  ]


elmApi modName (Module (ApiDecl kind type_ idField) types _) =
  let
      endpoint =
          toLowercase type_

      exposing =
          concat
          [ [ "get"
            , "update"
            , "delete"
            , "create"
            , "list"
            ]
          , Map.keys types
          , Map.keys types
            & map (\ typeName -> "default" + typeName)
          , Map.keys types
            & map (\ typeName -> "encode" + typeName)
          , Map.keys types
            & map (\ typeName -> toLowercase typeName + "Decoder")
          ]

      (+) =
          Text.append

      indexTypeRep =
          types
          & Map.lookup type_
          & Maybe.maybe (error "indexTypeRep")
            ( \ type_ ->
              case type_ of
                  ( TypeDecl _ fields ) ->
                      fields
                      & Prelude.filter ( (==) idField . Prelude.fst )
                      & ( \xs ->
                            case xs of
                                [ ( _, typeRep ) ] -> typeRep
                                _ -> error "indexTypeRep"
                        )
            )

      get =
          case kind of
              "ncms" ->
                  Text.unlines
                  [ "get : " + showTypeRep indexTypeRep + " -> Task Error " + type_
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  Text.unlines
                  [ "get : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "get ="
                  , "    Backend.get \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      delete =
          case kind of
              "ncms" ->
                  Text.unlines
                  [ "delete : " + showTypeRep indexTypeRep + " -> Task Error ()"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  Text.unlines
                  [ "delete : (Result Error () -> msg) -> String -> String -> String -> " + showTypeRep indexTypeRep + " -> Cmd msg"
                  , "delete ="
                  , "    Backend.delete \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              _ ->
                  error "unsupported kind"

      update =
          case kind of
              "ncms" ->
                  Text.unlines
                  [ "update : " + type_ + " -> Task Error ()"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  Text.unlines
                  [ "update : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "update ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      create =
          case kind of
              "ncms" ->
                  Text.unlines
                  [ "create : " + type_ + " -> Task Error ()"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              "github" ->
                  Text.unlines
                  [ "create : (Result Error " + type_ + " -> msg) -> String -> String -> String -> " + type_ + " -> Cmd msg"
                  , "create ="
                  , "    Backend.create \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder ." + idField
                  ]
              _ ->
                  error "unsupported kind"

      list =
          case kind of
              "ncms" ->
                  Text.unlines
                  [ "list : Task Error (List " + type_ + ")"
                  , "list ="
                  , "    Backend.list \"" + endpoint + "\" encode" + type_ + " " + toLowercase type_ + "Decoder"
                  ]
              "github" ->
                  Text.unlines
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
          & Text.intercalate "\n\n"
          & Prelude.flip Text.append "\n\n"

      encoders =
          let

              encodeTypeRep typeRep =
                  case typeRep of
                      TString -> "Encode.string"
                      TBool -> "Encode.bool"
                      TInt -> "Encode.int"
                      TFloat -> "Encode.float"
                      TMaybe typeRep ->
                          "Maybe.withDefault Encode.null <| Maybe.map "
                          + encodeTypeRep typeRep
                      TList typeRep ->
                          "Encode.list <| List.map " + encodeTypeRep typeRep

              decodeTypeRep (TypeDecl typeName fields)  =
                  Text.unlines
                  [ "encode" + typeName + " : " + typeName + " -> Decode.Value"
                  , "encode" + typeName + " value ="
                  , fields
                    & map (\ ( fieldName, typeRep ) ->
                        "( \"" + fieldName + "\", "
                        + encodeTypeRep typeRep  + " value." + fieldName + " )"
                      )
                    & Text.intercalate "\n    , "
                    & Text.append "    [ "
                    & Prelude.flip Text.append "\n    ]"
                  , "    |> Encode.object"
                  ]
          in
          Map.elems types
          & map decodeTypeRep
          & Text.intercalate "\n\n"
          & Prelude.flip Text.append "\n\n"

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
                  Text.unlines
                  [ toLowercase typeName + "Decoder : Decoder " + typeName
                  , toLowercase typeName + "Decoder ="
                  , fields
                    & map (\ ( fieldName, typeRep ) ->
                        "        ( Decode.at [ \"" + fieldName + "\" ] "
                        + typeRepDecoder typeRep  + " )"
                      )
                    & Text.unlines
                    & Text.append (
                          if Prelude.length fields == 1 then
                              "    Decode.map " + typeName + "\n"
                          else
                              "    Decode.map" + Text.pack (show (Prelude.length fields)) + " " + typeName + "\n"
                      )
                  ]
          in
          Map.elems types
          & map decodeTypeRep
          & Text.intercalate "\n\n"
          & Prelude.flip Text.append "\n\n"

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
          & map ( \ (TypeDecl typeName fields) ->
                Text.unlines
                [ "type alias "  + typeName + " ="
                , fields
                  & map (\ (fieldName, typeRep) ->
                        fieldName + " : " + showTypeRep typeRep
                    )
                  & Text.intercalate "\n    , "
                  & Text.append "    { "
                  & Prelude.flip Text.append "\n    }"
                ]
            )
          & Text.intercalate "\n\n"

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
          & map ( \ (TypeDecl typeName fields) ->
                Text.unlines
                [ "default" + typeName + " : " + typeName
                , "default" + typeName + " ="
                , fields
                  & map (\ (fieldName, typeRep) ->
                        fieldName + " = " + typeRepDefault typeRep
                    )
                  & Text.intercalate "\n    , "
                  & Text.append "    { "
                  & Prelude.flip Text.append "\n    }"
                ]
            )
          & Text.intercalate "\n\n"
  in
  Text.unlines
  [ "module " + modName + " exposing"
  , "    ( "
  ,
    exposing
    & Text.intercalate "\n    , "
    & Text.append "      "

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


readFile :: Prelude.FilePath -> IO String
readFile =
  fmap Text.pack . Prelude.readFile


writeFile :: Prelude.FilePath -> String -> IO ()
writeFile fileName contents =
  Prelude.writeFile fileName (Text.unpack contents)


data Module =
  Module ApiDecl (Map String TypeDecl) String deriving Show


fromExprs :: List Expr -> Either String Module
fromExprs exprs =
  let
      (apiDecls, typeDecls, garbage) =
          foldl
            (\(apiDecls, typeDecls, garbage') expr ->
                case expr of
                    ETypeDecl typeDecl ->
                        ( apiDecls, typeDecl:typeDecls, garbage' )
                    EApiDecl apiDecl ->
                        ( apiDecl:apiDecls, typeDecls, garbage' )
                    EEof garbage ->
                        ( apiDecls, typeDecls,
                              garbage
                              & Text.append garbage'
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
                    & map (\ typeDecl ->
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
              fmap (singleton . EEof) (Parser.takeText)


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
  & fmap Text.pack
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


-- PRELUDE


type String
  = Text.Text


foldl =
  Prelude.foldl


type List a
  = [a]


void =
  const ()


const =
  Prelude.const


fmap =
  Prelude.fmap


print =
  Prelude.print


map =
  Prelude.map


($) =
  (Prelude.$)


(.) =
  (Prelude..)


error =
  Prelude.error


pure =
  Prelude.return


type FilePath
  = Prelude.FilePath


singleton x =
  [x]


head xs =
  case xs of
      [] -> Nothing
      ( x : _ ) -> Just x


(/=) =
  (Prelude./=)


(==) =
    (Prelude.==)


toLowercase str =
    case Text.uncons str of
        Just (firstLetter, rest) ->
          Text.toLower (Text.pack (singleton firstLetter)) `Text.append` rest
        Nothing ->
            str


debug s x =
    unsafePerformIO (Prelude.putStrLn (s Prelude.++ show x))
        `Prelude.seq` x
