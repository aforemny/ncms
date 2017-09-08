module Ncms.Backend.Ncms exposing
    ( get
    , update
    , delete
    , list
    , create

    , idField
    )

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder, Value)
import Task exposing (Task)


get : String -> (a -> Value) -> Decoder a -> String -> Task Error a
get endpoint encode decoder id =
    Http.toTask <|
    request "GET" (endpoint ++ "/" ++ id) Http.emptyBody decoder


update : String -> (a -> Value) -> Decoder a -> (a -> String) -> a -> Task Error ()
update endpoint encode decoder id obj =
    Http.toTask <|
    request "POST" (endpoint ++ "/" ++ id obj) (Http.jsonBody (encode obj))
       ( Json.succeed () )


delete : String -> (a -> Value) -> Decoder a -> String -> Task Error ()
delete endpoint encode decoder id =
    Http.toTask <|
    request "DELETE" (endpoint ++ "/" ++ id) Http.emptyBody (Json.succeed ())


list : String -> (a -> Value) -> Decoder a -> Task Error (List a)
list endpoint encode decoder =
    Http.toTask <|
    request "GET" (endpoint) Http.emptyBody (Json.list decoder)


create : String -> (a -> Value) -> Decoder a -> (a -> String) -> a -> Task Error ()
create endpoint encode decoder _ obj =
    Http.toTask <|
    request "POST" (endpoint) (Http.jsonBody (encode obj)) (Json.succeed ())


request : String -> String -> Http.Body -> Decoder a -> Http.Request a
request method url body decoder =
    Http.request
    { method = method
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }


idField : String -> Value -> String
idField field obj =
    case Json.decodeValue (Json.at [ field ] Json.string) obj of
        Ok x ->
            x
        Err e ->
            Debug.crash ("Ncms.Backend.Github.idField: " ++ e)
