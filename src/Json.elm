module Json exposing (Json(..), decoder)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)


type Json
    = List (List Json)
    | Int Int
    | Float Float
    | String String
    | Bool Bool
    | Null
    | Object (Dict String Json)


decoder : Decoder Json
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map Object
            (Json.Decode.dict (Json.Decode.lazy (\_ -> decoder)))
        , Json.Decode.map List
            (Json.Decode.list (Json.Decode.lazy (\_ -> decoder)))
        , Json.Decode.map Int Json.Decode.int
        , Json.Decode.map String Json.Decode.string
        , Json.Decode.map Float Json.Decode.float
        , Json.Decode.map Bool Json.Decode.bool
        , Json.Decode.null Null
        ]
