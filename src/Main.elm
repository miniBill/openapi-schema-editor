module Main exposing (main)

import Browser
import Cmd.Extra exposing (add)
import Dict exposing (Dict)
import Html exposing (Html, div, i, main_, select, textarea)
import Html.Attributes exposing (checked, rows, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Maybe.Extra
import Regex exposing (Regex)
import Result.Extra
import Theme
import Type exposing (AdditionalProperties(..), Type(..))


type alias Model =
    { input : String
    , json : Result Json.Decode.Error Json
    , type_ : Type
    }


type Msg
    = Input String
    | Type Type
    | DownloadedJson (Result Http.Error Json)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , json = Json.Decode.decodeString jsonDecoder ""
      , type_ = TInteger
      }
    , Http.get
        { url = "big.json"
        , expect = Http.expectJson DownloadedJson jsonDecoder
        }
    )


type Json
    = List (List Json)
    | Int Int
    | Float Float
    | String String
    | Bool Bool
    | Null
    | Object (Dict String Json)


jsonDecoder : Decoder Json
jsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Object
            (Json.Decode.dict (Json.Decode.lazy (\_ -> jsonDecoder)))
        , Json.Decode.map List
            (Json.Decode.list (Json.Decode.lazy (\_ -> jsonDecoder)))
        , Json.Decode.map Int Json.Decode.int
        , Json.Decode.map String Json.Decode.string
        , Json.Decode.map Float Json.Decode.float
        , Json.Decode.map Bool Json.Decode.bool
        , Json.Decode.null Null
        ]


view : Model -> Html Msg
view model =
    main_
        [ style "display" "flex"
        , style "padding" "8px"
        , style "gap" "8px"
        ]
        [ textarea
            [ value model.input
            , onInput Input
            , style "flex" "1"
            , style "height" "calc(100dvh - 16px)"
            ]
            []
        , div
            [ style "flex" "1" ]
            [ Html.map Type (Type.editor model.type_) ]
        , div
            [ style "flex" "1" ]
            [ case model.json of
                Err e ->
                    Html.text (Debug.toString e)

                Ok json ->
                    viewMatch (matches model.type_ json)
            ]
        ]


viewMatch : Result (Html Type) () -> Html Msg
viewMatch match =
    case match of
        Ok () ->
            Html.text "All matches"

        Err e ->
            Html.map Type e


matches : Type -> Json -> Result (Html Type) ()
matches t j =
    let
        mismatch : Maybe Type -> Result (Html Type) ()
        mismatch maybeSuggested =
            let
                suggested : Type
                suggested =
                    case maybeSuggested of
                        Just s ->
                            s

                        Nothing ->
                            suggestType j
            in
            Html.div
                [ style "padding" "2px"
                , style "border" "1px solid gray"
                ]
                [ Html.button [ onClick suggested ] [ Html.text ("Use " ++ Type.toString suggested ++ " instead") ]
                , Html.br [] []
                , Html.text
                    ("Expected "
                        ++ Type.toString t
                        ++ ", got "
                        ++ Json.Encode.encode 0 (encodeJson j)
                    )
                ]
                |> Err
    in
    case ( t, j ) of
        ( TOneOf alternatives, _ ) ->
            case
                List.Extra.find
                    (\alternative ->
                        matches alternative j
                            |> Result.Extra.isOk
                    )
                    alternatives
            of
                Nothing ->
                    mismatch
                        (Just
                            (TOneOf
                                (alternatives
                                    ++ [ suggestType j ]
                                )
                            )
                        )

                Just _ ->
                    Ok ()

        ( TList tchild, List children ) ->
            children
                |> Result.Extra.combineMap (\child -> matches tchild child)
                |> Result.map (\_ -> ())

        ( TString { pattern }, String s ) ->
            case pattern of
                Nothing ->
                    Ok ()

                Just p ->
                    let
                        regex : Regex
                        regex =
                            Regex.fromString p
                                |> Maybe.withDefault Regex.never
                    in
                    if Regex.contains regex s then
                        Ok ()

                    else
                        mismatch Nothing

        ( TInteger, Int _ ) ->
            Ok ()

        ( TNumber, Float _ ) ->
            Ok ()

        ( TBoolean, Bool _ ) ->
            Ok ()

        ( TNull, Null ) ->
            Ok ()

        ( TObject { fields, additionalProperties }, Object v ) ->
            let
                fieldsDict =
                    Dict.fromList fields
            in
            v
                |> Dict.toList
                |> Result.Extra.combineMap
                    (\( fieldName, fieldValue ) ->
                        case Dict.get fieldName fieldsDict of
                            Just field ->
                                matches field.type_ fieldValue
                                    |> Result.mapError
                                        (Html.map
                                            (\suggestedType ->
                                                TObject
                                                    { fields =
                                                        List.Extra.setIf
                                                            (\( n, _ ) -> n == fieldName)
                                                            ( fieldName, { field | type_ = suggestedType } )
                                                            fields
                                                    , additionalProperties = additionalProperties
                                                    }
                                            )
                                        )

                            Nothing ->
                                case additionalProperties of
                                    AdditionalPropertiesNotAllowed ->
                                        mismatch
                                            (Just
                                                (TObject
                                                    { fields =
                                                        fields
                                                            ++ [ ( fieldName
                                                                 , { type_ = suggestType fieldValue
                                                                   , required = True
                                                                   , nullable = False
                                                                   }
                                                                 )
                                                               ]
                                                    , additionalProperties = additionalProperties
                                                    }
                                                )
                                            )

                                    AdditionalPropertiesAllowed Nothing ->
                                        Ok ()

                                    AdditionalPropertiesAllowed (Just additionalType) ->
                                        matches additionalType fieldValue
                                            |> Result.mapError
                                                (Html.map
                                                    (\suggestedType ->
                                                        TObject
                                                            { fields =
                                                                fields
                                                                    ++ [ ( fieldName
                                                                         , { type_ = suggestType fieldValue
                                                                           , required = True
                                                                           , nullable = False
                                                                           }
                                                                         )
                                                                       ]
                                                            , additionalProperties = additionalProperties
                                                            }
                                                    )
                                                )
                    )
                |> Result.map (\_ -> ())

        _ ->
            mismatch Nothing


suggestType : Json -> Type
suggestType j =
    case j of
        List [] ->
            TList TNull

        List (h :: t) ->
            t
                |> List.foldl
                    (\e a -> Type.union (suggestType e) a)
                    (suggestType h)
                |> TList

        Int _ ->
            TInteger

        Float _ ->
            TNumber

        String _ ->
            TString { pattern = Nothing }

        Bool _ ->
            TBoolean

        Null ->
            TNull

        Object fields ->
            TObject
                { fields =
                    fields
                        |> Dict.toList
                        |> List.map
                            (\( fieldName, fieldValue ) ->
                                ( fieldName
                                , { type_ = suggestType fieldValue
                                  , required = True
                                  , nullable = False
                                  }
                                )
                            )
                , additionalProperties = AdditionalPropertiesNotAllowed
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            ( { model
                | input = input
                , json = Json.Decode.decodeString jsonDecoder input
              }
            , Cmd.none
            )

        Type type_ ->
            ( { model | type_ = type_ }, Cmd.none )

        DownloadedJson (Err (Http.BadBody e)) ->
            ( { model | input = e }, Cmd.none )

        DownloadedJson (Err e) ->
            ( { model | input = Debug.toString e }, Cmd.none )

        DownloadedJson (Ok json) ->
            ( { model
                | json = Ok json
                , input = Json.Encode.encode 2 (encodeJson json)
              }
            , Cmd.none
            )


encodeJson : Json -> Json.Encode.Value
encodeJson json =
    case json of
        List items ->
            Json.Encode.list encodeJson items

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
                |> List.map (\( k, v ) -> ( k, encodeJson v ))
                |> Json.Encode.object


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
