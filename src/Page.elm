module Page exposing
    ( Page(..)
    , ApiId(..)
    , DataId(..)
    , defaultPage
    , fromHash
    , toHash
    )


type Page
    = Dashboard
    | Listing ApiId
    | Edit ApiId DataId
    | New ApiId
    | NotFound String


type ApiId =
    ApiId String


type DataId =
    DataId String


defaultPage : Page
defaultPage =
    Dashboard


fromHash : String -> Page
fromHash hash =
    case String.uncons hash of
        Nothing ->
            Dashboard
        Just ('#', "") ->
            Dashboard
        Just ('#', rest) ->
            case String.split "/" rest of
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
