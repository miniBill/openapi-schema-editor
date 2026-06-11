module Main exposing (main)

import Browser
import Cmd.Extra exposing (add)
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, i, main_, p, pre, select, text, textarea)
import Html.Attributes exposing (checked, rows, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json exposing (Json(..))
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Maybe.Extra
import Parser.Advanced
import Regex exposing (Regex)
import Result.Extra
import Rfc3339
import Set exposing (Set)
import String.Extra
import Theme
import Type exposing (AdditionalProperties(..), Type(..))
import Url


type alias Model =
    { input : String
    , json : Result Json.Decode.Error Json
    , types : Dict String Type
    }


type Msg
    = Input String
    | Type String Type
    | RenameType String String
    | DownloadedJson (Result Http.Error Json)
    | AddType
    | RemoveType String


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
      , json = Json.Decode.decodeString Json.decoder ""
      , types = Dict.empty
      }
    , Http.get
        { url = "big.json"
        , expect = Http.expectJson DownloadedJson Json.decoder
        }
    )


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
            , style "font-family" "monospace"
            ]
            []
        , div
            [ style "flex" "1"
            , style "display" "grid"
            , style "grid-template-columns" "auto 1fr auto"
            , style "align-self" "start"
            , style "gap" "4px"
            ]
            (viewTypes model.types)
        , div
            [ style "flex" "1" ]
            [ case model.json of
                Err e ->
                    e
                        |> Json.Decode.errorToString
                        |> String.split "\n"
                        |> List.map
                            (\line ->
                                p
                                    [ style "font-family" "monospace" ]
                                    [ text line ]
                            )
                        |> Html.div []

                Ok json ->
                    viewMatches model.types json
            ]
        ]


viewTypes : Dict String Type -> List (Html Msg)
viewTypes types =
    let
        typeNames : List String
        typeNames =
            Dict.keys types
    in
    button
        [ onClick AddType
        , style "grid-column" "1 / span 3"
        ]
        [ text "➕ New type" ]
        :: (types
                |> Dict.update "" (Maybe.withDefault TNull >> Just)
                |> Dict.toList
                |> List.concatMap (viewType typeNames)
           )


viewType : List String -> ( String, Type ) -> List (Html Msg)
viewType typeNames ( name, type_ ) =
    [ if String.isEmpty name then
        Html.p [] [ Html.text "<root>" ]

      else
        Html.input [ onInput (RenameType name), value name ] []
    , Html.map (Type name) (Type.editor typeNames type_)
    , if String.isEmpty name then
        Html.div [] []

      else
        button [ onClick (RemoveType name) ] [ text "🗑️" ]
    ]


viewMatches : Dict String Type -> Json -> Html Msg
viewMatches types j =
    let
        dict : Dict String ( Maybe Type, List Json )
        dict =
            buildDict Set.empty types "" j Dict.empty
    in
    dict
        |> Dict.toList
        |> List.concatMap
            (\( typeName, ( type_, values ) ) ->
                let
                    problems : List (Html Type)
                    problems =
                        findProblems type_ values
                in
                if List.isEmpty problems then
                    []

                else
                    [ Html.div []
                        [ if String.isEmpty typeName then
                            Html.text "<root>"

                          else
                            Html.text typeName
                        ]
                    , Html.div [] problems
                        |> Html.map (Type typeName)
                    ]
            )
        |> div
            [ style "display" "grid"
            , style "gap" "4px"
            , style "grid-template-columns" "auto 1fr"
            ]


buildDict : Set String -> Dict String Type -> String -> Json -> Dict String ( Maybe Type, List Json ) -> Dict String ( Maybe Type, List Json )
buildDict seen types typeName value acc =
    if Set.member typeName seen then
        acc

    else
        let
            ( maybeType, updated ) =
                case Dict.get typeName acc of
                    Nothing ->
                        let
                            t =
                                Dict.get typeName types
                        in
                        ( t, Dict.insert typeName ( t, [ value ] ) acc )

                    Just ( t, list ) ->
                        ( t
                        , Dict.insert typeName
                            ( t
                            , if List.member value list then
                                list

                              else
                                value :: list
                            )
                            acc
                        )

            go t child innerAcc =
                case ( t, child ) of
                    ( TList c, List children ) ->
                        List.foldl (go c) innerAcc children

                    ( TObject { fields }, Object fs ) ->
                        Dict.merge
                            (\_ _ a -> a)
                            (\fieldName field fieldValue a -> go field.type_ fieldValue a)
                            (\_ _ a -> a)
                            (Dict.fromList fields)
                            fs
                            innerAcc

                    ( TOneOf opts, _ ) ->
                        case List.Extra.find (\opt -> Type.isValidFor opt child) opts of
                            Just c ->
                                go c child innerAcc

                            Nothing ->
                                List.foldl (\opt a -> go opt child a) innerAcc opts

                    ( TRef ref, _ ) ->
                        buildDict (Set.insert typeName seen) types ref child innerAcc

                    _ ->
                        innerAcc
        in
        case maybeType of
            Nothing ->
                updated

            Just t ->
                go t value updated


findProblems : Maybe Type -> List Json -> List (Html Type)
findProblems maybeType js =
    let
        ( matching, nonMatching ) =
            case maybeType of
                Nothing ->
                    ( [], js )

                Just t ->
                    List.partition (\j -> Type.isValidFor t j) js
    in
    case nonMatching of
        [] ->
            []

        jsHead :: _ ->
            let
                paragraph : List (Html msg) -> Html msg
                paragraph children =
                    p
                        [ style "font-family" "monospace"
                        , style "overflow-wrap" "break-word"
                        , style "max-width" "40vw"
                        , style "white-space" "pre-wrap"
                        ]
                        children

                mismatch : Maybe String -> Type -> List (Html Type)
                mismatch problem suggested =
                    [ div
                        [ style "padding" "2px"
                        , style "border" "1px solid gray"
                        , style "display" "flex"
                        , style "flex-direction" "column"
                        , style "gap" "4px"
                        ]
                        [ case problem of
                            Nothing ->
                                Html.text ""

                            Just problemString ->
                                paragraph [ text problemString ]
                        , case maybeType of
                            Just t ->
                                paragraph [ text "Expected  ", text (Type.toString t) ]

                            Nothing ->
                                paragraph [ text "Unknown type" ]
                        , paragraph
                            [ text "Suggested ", text (String.Extra.ellipsis 300 (Type.toString suggested)) ]
                        , button [ onClick suggested ]
                            [ text "Use suggested instead" ]
                        , paragraph
                            [ text "Got "
                            , jsHead
                                |> cut
                                |> encodeJson
                                |> Json.Encode.encode 0
                                |> String.Extra.ellipsis 1000
                                |> text
                            ]
                        ]
                    ]
            in
            case ( maybeType, jsHead ) of
                -- ( TOneOf alternatives, _ ) ->
                --     case List.Extra.find (\alternative -> Type.isValidFor alternative j) alternatives of
                --         Nothing ->
                --             mismatch path (Just t) j <|
                --                 Just
                --                     (TOneOf
                --                         (alternatives
                --                             ++ [ suggestType j ]
                --                         )
                --                     )
                --         Just _ ->
                --             []
                -- ( TList tchild, List children ) ->
                --     children
                --         |> List.indexedMap
                --             (\i child ->
                --                 matchesHelp
                --                     (String.fromInt i :: path)
                --                     tchild
                --                     child
                --             )
                --         |> List.concat
                -- ( TString ({ pattern, const, format } as str), String s ) ->
                --     case Type.matchesString str s of
                --         Err (Type.IsNotConst _) ->
                --             mismatch path (Just t) j (Just (TString { str | const = Just s }))
                --         Err Type.DoesNotMatchPattern ->
                --             mismatch path (Just t) j Nothing
                --         Err (Type.UnknownFormat f) ->
                --             div
                --                 [ style "padding" "2px"
                --                 , style "border" "1px solid gray"
                --                 ]
                --                 [ text ("Unknown format: " ++ f) ]
                --                 |> Err
                --         Err (Type.DoesNotMatchFormat f) ->
                --             div
                --                 [ style "padding" "2px"
                --                 , style "border" "1px solid gray"
                --                 ]
                --                 [ text
                --                     ("At "
                --                         ++ String.join "." (List.reverse path)
                --                         ++ ", expected "
                --                         ++ Type.toString t
                --                         ++ ", got "
                --                         ++ Json.Encode.encode 0 (encodeJson j)
                --                     )
                --                 ]
                --                 |> Err
                --         Ok () ->
                --             []
                -- ( TInteger, Int _ ) ->
                --     []
                -- ( TNumber, Float _ ) ->
                --     []
                -- ( TBoolean, Bool _ ) ->
                --     []
                -- ( TNull, Null ) ->
                --     []
                ( Just (TObject data), Object v ) ->
                    case Type.matchesObject data v of
                        [] ->
                            []

                        (Type.UnexpectedField fieldName { found }) :: _ ->
                            mismatch
                                (Just ("Unexpected field " ++ fieldName))
                                (TObject
                                    { data
                                        | fields =
                                            data.fields
                                                ++ [ ( fieldName
                                                     , { type_ = suggestType found
                                                       , required = True
                                                       , nullable = False
                                                       }
                                                     )
                                                   ]
                                    }
                                )

                        (Type.WrongFieldType fieldName { expected, found }) :: _ ->
                            mismatch
                                (Just ("Wrong type for field " ++ fieldName))
                                (TObject
                                    { data
                                        | fields =
                                            data.fields
                                                |> List.Extra.updateIf
                                                    (\( name, _ ) -> name == fieldName)
                                                    (\( name, field ) ->
                                                        ( name
                                                        , { field | type_ = suggestType found }
                                                        )
                                                    )
                                    }
                                )

                _ ->
                    mismatch Nothing (suggestType jsHead)


cut : Json -> Json
cut j =
    case j of
        List l ->
            List (List.map cut l)

        String s ->
            String (String.Extra.ellipsis 30 s)

        Object fields ->
            Object (Dict.map (always cut) fields)

        _ ->
            j


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

        String s ->
            case Parser.Advanced.run Rfc3339.dateTimeOffsetParser s of
                Ok _ ->
                    TString { pattern = Nothing, const = Nothing, format = Just "date-time" }

                Err _ ->
                    case Url.fromString s of
                        Just _ ->
                            TString { pattern = Nothing, const = Nothing, format = Just "uri" }

                        Nothing ->
                            TString { pattern = Nothing, const = Nothing, format = Nothing }

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
                                case ( fieldName, fieldValue ) of
                                    ( "type", String s ) ->
                                        ( fieldName
                                        , { type_ = TString { format = Nothing, pattern = Nothing, const = Just s }
                                          , required = True
                                          , nullable = False
                                          }
                                        )

                                    _ ->
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
                , json = Json.Decode.decodeString Json.decoder input
              }
            , Cmd.none
            )

        AddType ->
            let
                newKey : String
                newKey =
                    let
                        typeNames : Set String
                        typeNames =
                            Dict.keys model.types |> Set.fromList

                        go n =
                            let
                                candidate : String
                                candidate =
                                    "t" ++ String.fromInt n
                            in
                            if Set.member candidate typeNames then
                                go (n + 1)

                            else
                                candidate
                    in
                    go 1
            in
            ( { model | types = Dict.insert newKey TNull model.types }, Cmd.none )

        Type key type_ ->
            ( { model | types = Dict.insert key type_ model.types }, Cmd.none )

        RenameType old new ->
            case ( Dict.get old model.types, Dict.get new model.types ) of
                ( Just t, Nothing ) ->
                    ( { model
                        | types =
                            model.types
                                |> Dict.remove old
                                |> Dict.insert new t
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RemoveType name ->
            ( { model | types = Dict.remove name model.types }, Cmd.none )

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
