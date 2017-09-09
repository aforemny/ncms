module Page exposing
    ( Page(..)
    , ApiId(..)
    , DataId(..)
    , defaultPage
    , fromHash
    , toHash
    )


type Page
    = NotFound String
    | Dashboard
    | Listing ApiId
    | Edit ApiId DataId
    | New ApiId
    | Image (Maybe String)


type ApiId =
    ApiId String


type DataId =
    DataId String


defaultPage : Page
defaultPage =
    Dashboard


fromHash : String -> Page
fromHash hash =
    case String.split "/" hash of
        [] ->
            Dashboard
        (""::[]) ->
            Dashboard
        ("#"::[]) ->
            Dashboard
        ("#image"::[]) ->
            Image Nothing
        ("#image"::folder) ->
            Image (Just (String.join "/" folder))
        (head::rest) ->
            case String.uncons head of
                Just ('#', api) ->
                    case rest of
                      (api::"new"::_) ->
                          New (ApiId api)
                      (api::"edit"::id::_) ->
                          Edit (ApiId api) (DataId id)
                      (api::_) ->
                          Listing (ApiId api)
                      _ ->
                          NotFound hash
                _ ->
                    NotFound hash

toHash : Page -> String
toHash page =
    case page of
        Dashboard ->
            "#"
        New (ApiId api) ->
            "#" ++ api ++ "/new"
        Edit (ApiId api) (DataId id) ->
            "#" ++ api ++ "/edit/" ++ id
        Listing (ApiId api) ->
            "#" ++ api
        NotFound hash ->
            hash
        Image Nothing ->
            "#image"
        Image (Just folder) ->
            "#image/" ++ folder
