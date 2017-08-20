module Ncms.Backend.Github exposing
    ( get
    , update
    , delete
    , list
    , create
    )

import Http exposing (Error)
import Json.Decode as Json


get endpoint decoder cont id =
    Http.send cont <|
    request "GET" ("/" ++ endpoint ++ "/" ++ id) Http.emptyBody decoder


update endpoint encode decoder id cont obj =
    Http.send cont <|
    request "POST" ("/" ++ endpoint ++ "/" ++ id obj) (Http.jsonBody (encode obj)) decoder


delete endpoint decoder cont id =
    Http.send cont <|
    request "DELETE" ("/" ++ endpoint ++ "/" ++ id) Http.emptyBody decoder


list endpoint cont decoder =
    Http.send cont <|
    request "GET" ("/" ++ endpoint) Http.emptyBody (Json.list decoder)


create endpoint encode decoder cont obj =
    Http.send cont <|
    request "POST" ("/" ++ endpoint) (Http.jsonBody (encode obj)) decoder


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
