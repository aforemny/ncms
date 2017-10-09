module Parser where

import Core
import Data.Attoparsec.Text (Parser)
import qualified Config
import qualified Core.Dict as Dict
import qualified Core.Directory as Directory
import qualified Core.List as List
import qualified Core.String as String
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Prelude


findApiFiles config = do
  liftIO (Directory.findFiles (Config.dataDirectory config))
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


parseApiFile apiFile = do
  parsedApiFile <- fmap parse (String.readFile apiFile)
  case parsedApiFile of
      Right parsedApiFile ->
          return parsedApiFile
      Left e ->
          error (String.unpack e)


data Module =
  Module ApiDecl (Dict String TypeDecl) String deriving Show


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
                    & Dict.fromList
                  )
                  garbage

          (_, _) ->
              Left $ "more than one api decl"


parse :: String -> Either String Module
parse input =
    fromExprs $
    case Attoparsec.parseOnly exprs input of
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
              fmap (:) (pure token) <*> exprs
          Nothing ->
              fmap (List.singleton . EEof) (Attoparsec.takeText)


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
  many Attoparsec.letter
  & fmap String.pack
  & lexeme


typeDecl :: Parser TypeDecl
typeDecl =
  TypeDecl
      <$> (string "type" *> typeName)
      <*> ( do Attoparsec.char '='
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
  Attoparsec.string str


lexeme :: Parser a -> Parser a
lexeme parser =
  parser <* many (fmap void space <|> Attoparsec.endOfLine)


space :: Parser ()
space =
  oneOf [ Attoparsec.char ' ', Attoparsec.char '\t' ]
  & fmap void


many :: Parser a -> Parser (List a)
many =
  Attoparsec.many'


oneOf :: List (Parser a) -> Parser a
oneOf =
  Attoparsec.choice


fail =
    Prelude.fail
