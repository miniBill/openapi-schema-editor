module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, main_, select, textarea)
import Html.Attributes exposing (checked, rows, style, type_, value)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Theme


type alias Model =
    { input : String
    , json : Result Json.Decode.Error Json
    , type_ : Type
    }


type Msg
    = Input String
    | Type Type
    | DownloadedJson (Result Http.Error Json)


type Type
    = TList Type
    | TString { pattern : Maybe String }
    | TInteger
    | TNumber
    | TBoolean
    | TNull
    | TObject
        { fields : List ( String, Type )
        , additionalProperties : AdditionalProperties
        }


type AdditionalProperties
    = AdditionalPropertiesAllowed (Maybe Type)
    | AdditionalPropertiesNotAllowed


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
            [ Html.map Type (typeEditor model.type_) ]
        ]


typeEditor : Type -> Html Type
typeEditor t =
    let
        default =
            { obj = Nothing
            , list = Nothing
            , string = Nothing
            }

        ( extracted, additional ) =
            case t of
                TObject obj ->
                    let
                        fieldsViews : List (Html Type)
                        fieldsViews =
                            List.indexedMap viewField obj.fields

                        viewField : Int -> ( String, Type ) -> Html Type
                        viewField i ( fieldName, fieldType ) =
                            typeEditor fieldType
                                |> Html.map
                                    (\newType ->
                                        TObject
                                            { obj
                                                | fields =
                                                    List.Extra.setAt i
                                                        ( fieldName, newType )
                                                        obj.fields
                                            }
                                    )

                        additionalPropertiesViews : List (Html Type)
                        additionalPropertiesViews =
                            [ Theme.select
                                [ ( "No additional properties"
                                  , TObject
                                        { obj
                                            | additionalProperties = AdditionalPropertiesNotAllowed
                                        }
                                  )
                                , ( "Additional properties allowed (any)"
                                  , TObject
                                        { obj
                                            | additionalProperties = AdditionalPropertiesAllowed Nothing
                                        }
                                  )
                                , ( "Additional properties allowed (specific)"
                                  , TObject
                                        { obj
                                            | additionalProperties =
                                                AdditionalPropertiesAllowed
                                                    (case obj.additionalProperties of
                                                        AdditionalPropertiesAllowed (Just v) ->
                                                            Just v

                                                        AdditionalPropertiesAllowed Nothing ->
                                                            Nothing

                                                        AdditionalPropertiesNotAllowed ->
                                                            Nothing
                                                    )
                                        }
                                  )
                                ]
                                t
                            ]
                    in
                    ( { default | obj = Just obj }
                    , fieldsViews ++ additionalPropertiesViews
                    )

                TList list ->
                    ( { default | list = Just list }
                    , [ Html.map TList (typeEditor list) ]
                    )

                TString str ->
                    ( { default | string = Just str }
                    , Html.input
                        [ type_ "checkbox"
                        , checked (str.pattern /= Nothing)
                        , onCheck
                            (\newValue ->
                                TString
                                    { str
                                        | pattern =
                                            if newValue then
                                                Just ""

                                            else
                                                Nothing
                                    }
                            )
                        ]
                        []
                    , Html.input
                        [ onInput
                            (\newPattern ->
                                TString
                                    { str
                                        | pattern = Just newPattern
                                    }
                            )
                        , value (Maybe.withDefault "" str.pattern)
                        ]
                        []
                    )

                TInteger ->
                    ( default, [] )

                TNumber ->
                    ( default, [] )

                TBoolean ->
                    ( default, [] )

                TNull ->
                    ( default, [] )
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "4px"
        ]
        (Theme.select
            [ ( "object", TObject (Maybe.withDefault emptyObject extracted.obj) ) ]
            t
            :: additional
        )


emptyObject :
    { fields : List ( String, Type )
    , additionalProperties : AdditionalProperties
    }
emptyObject =
    { fields = []
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
