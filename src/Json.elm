module Json exposing (Json(..), decoder, encode)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode


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


encode : Json -> Json.Encode.Value
encode json =
    case json of
        List items ->
            Json.Encode.list encode items

        Int v ->
            Json.Encode.int v

        Float v ->
            Json.Encode.float v

        String v ->
            Json.Encode.string v

        Bool v ->
            Json.Encode.bool v

        Null ->
            Json.Encode.null

        Object d ->
            d
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, encode v ))
                |> Json.Encode.object
