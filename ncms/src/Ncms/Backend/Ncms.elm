module Ncms.Backend.Ncms exposing
    ( get
    , update
    , delete
    , list
    , create
    )

import Http exposing (Error)
import Json.Decode as Json exposing (Decoder, Value)


get : String -> (a -> Value) -> Decoder a -> (Result Error a -> msg) -> String -> Cmd msg
get endpoint encode decoder cont id =
    Http.send cont <|
    request "GET" (endpoint ++ "/" ++ id) Http.emptyBody decoder


update : String -> (a -> Value) -> Decoder a -> (a -> String) -> (Result Error a -> msg) -> a -> Cmd msg
update endpoint encode decoder id cont obj =
    Http.send cont <|
    request "POST" (endpoint ++ "/" ++ id obj) (Http.jsonBody (encode obj)) decoder


delete : String -> (a -> Value) -> Decoder a -> (Result Error a -> msg) -> String -> Cmd msg
delete endpoint encode decoder cont id =
    Http.send cont <|
    request "DELETE" (endpoint ++ "/" ++ id) Http.emptyBody decoder


list : String -> (a -> Value) -> Decoder a -> (Result Error (List a) -> msg) -> Cmd msg
list endpoint encode decoder cont =
    Http.send cont <|
    request "GET" (endpoint) Http.emptyBody (Json.list decoder)


create : String -> (a -> Value) -> Decoder a -> (Result Error a -> msg) -> a -> Cmd msg
create endpoint encode decoder cont obj =
    Http.send cont <|
    request "POST" (endpoint) (Http.jsonBody (encode obj)) decoder


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
