module Api.Blog exposing
    ( 
      get
    , update
    , delete
    , create
    , list
    , Blog
    , defaultBlog
    , encodeBlog
    , blogDecoder
    )


import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ncms.Backend.Ncms as Backend


-- API


get : (Result Error Blog -> msg) -> String -> Cmd msg
get =
    Backend.get "blog" encodeBlog blogDecoder


update : (Result Error Blog -> msg) -> Blog -> Cmd msg
update =
    Backend.update "blog" encodeBlog blogDecoder .id


delete : (Result Error Blog -> msg) -> String -> Cmd msg
delete =
    Backend.delete "blog" encodeBlog blogDecoder


list : (Result Error (List Blog) -> msg) -> Cmd msg
list =
    Backend.list "/blog" encodeBlog blogDecoder


create : (Result Error Blog -> msg) -> Blog -> Cmd msg
create =
    Backend.create "blog" encodeBlog blogDecoder



-- TYPES


type alias Blog =
    { id : String
    , published : Bool
    , date : String
    , title : String
    , content : String
    }



defaultBlog : Blog
defaultBlog =
    { id = ""
    , published = False
    , date = ""
    , title = ""
    , content = ""
    }



-- DECODER


blogDecoder : Decoder Blog
blogDecoder =
    Decode.map5 Blog
        ( Decode.at [ "id" ] Decode.string )
        ( Decode.at [ "published" ] Decode.bool )
        ( Decode.at [ "date" ] Decode.string )
        ( Decode.at [ "title" ] Decode.string )
        ( Decode.at [ "content" ] Decode.string )




-- ENCODER


encodeBlog : Blog -> Decode.Value
encodeBlog value =
    [ ( "id", Encode.string value.id )
    , ( "published", Encode.bool value.published )
    , ( "date", Encode.string value.date )
    , ( "title", Encode.string value.title )
    , ( "content", Encode.string value.content )
    ]
    |> Encode.object



