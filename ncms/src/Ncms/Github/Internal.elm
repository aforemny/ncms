module Ncms.Github.Internal exposing
    (
      succeed
    , (|=)
    )

import Json.Decode as Json exposing (Decoder)


succeed : a -> Decoder a
succeed =
    Json.succeed


(|=)  : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) mf mx =
    Json.andThen (\f -> Json.map f mx ) mf


infixl 0 |=
