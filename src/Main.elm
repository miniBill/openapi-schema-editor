module Main exposing (main)

import Browser
import Codec exposing (Codec)
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, main_, p, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json exposing (Json(..))
import Json.Decode
import Json.Encode
import List.Extra
import Set exposing (Set)
import String.Extra
import Task
import Type exposing (Type(..))


type alias Model =
    { input : String
    , json : Result Json.Decode.Error Json
    , types : Dict String Type
    , selectedType : String
    }


type Msg
    = Input String
    | Type String Type
    | RenameType String String
    | DownloadedJson (Result Http.Error Json)
    | AddType
    | RemoveType String
    | Save
    | Load
    | SelectedFile File
    | ReadFile String


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
      , selectedType = ""
      }
    , Http.get
        { url = "big.json"
        , expect = Http.expectJson DownloadedJson Json.decoder
        }
    )


view : Model -> Html Msg
view model =
    main_
        [ style "display" "grid"
        , style "grid-template-columns" "auto auto auto"
        , style "padding" "8px"
        , style "gap" "8px"
        , style "height" "100dvh"
        ]
        [ div
            [ style "grid-column" "1 / span 3"
            ]
            [ button
                [ onClick Save
                ]
                [ text "Save" ]
            , text " "
            , button
                [ onClick Load
                ]
                [ text "Load" ]
            ]
        , textarea
            [ value model.input
            , onInput Input
            , style "font-family" "monospace"
            , style "height" "calc(100dvh - 48px)"
            , style "overflow-y" "scroll"
            ]
            []
        , div
            []
            [ div
                [ style "display" "grid"
                , style "grid-template-columns" "auto 1fr auto"
                , style "align-self" "start"
                , style "gap" "4px"
                ]
                (viewTypes model.types)
            , div []
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
                            |> div []

                    Ok json ->
                        viewMatches model.types json
                ]
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
        p [] [ text "<root>" ]

      else
        Html.input [ onInput (RenameType name), value name ] []
    , Html.map (Type name) (Type.editor typeNames type_)
    , if String.isEmpty name then
        div [] []

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
                    [ div []
                        [ if String.isEmpty typeName then
                            text "<root>"

                          else
                            text typeName
                        ]
                    , div [] problems
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
                            (\_ field fieldValue a -> go field.type_ fieldValue a)
                            (\_ _ a -> a)
                            (Dict.fromList fields)
                            fs
                            innerAcc

                    ( TOneOf opts, _ ) ->
                        case List.Extra.find (\opt -> Type.isValidFor opt child) opts of
                            Just c ->
                                go c child innerAcc

                            Nothing ->
                                let
                                    specific : Maybe Type
                                    specific =
                                        case child of
                                            Object fs ->
                                                case Dict.get "type" fs of
                                                    Just (String type_) ->
                                                        List.Extra.find
                                                            (\opt ->
                                                                case opt of
                                                                    TObject { fields } ->
                                                                        List.member
                                                                            ( "type"
                                                                            , { nullable = False
                                                                              , required = True
                                                                              , type_ = TString { const = Just type_, pattern = Nothing, format = Nothing }
                                                                              }
                                                                            )
                                                                            fields

                                                                    TList _ ->
                                                                        False

                                                                    TString _ ->
                                                                        False

                                                                    TInteger ->
                                                                        False

                                                                    TNumber ->
                                                                        False

                                                                    TBoolean ->
                                                                        False

                                                                    TNull ->
                                                                        False

                                                                    TOneOf _ ->
                                                                        False

                                                                    TRef _ ->
                                                                        False
                                                            )
                                                            opts

                                                    _ ->
                                                        Nothing

                                            List _ ->
                                                Nothing

                                            Int _ ->
                                                Nothing

                                            Float _ ->
                                                Nothing

                                            String _ ->
                                                Nothing

                                            Bool _ ->
                                                Nothing

                                            Null ->
                                                Nothing
                                in
                                case specific of
                                    Just s ->
                                        go s child innerAcc

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
        nonMatching : List Json
        nonMatching =
            case maybeType of
                Nothing ->
                    js

                Just t ->
                    List.Extra.removeWhen (\j -> Type.isValidFor t j) js
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
                                text ""

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
                                |> Json.encode
                                |> Json.Encode.encode 2
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
                --                             ++ [ Type.suggest j ]
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
                                (Just ("Unexpected field " ++ fieldName ++ " of type " ++ Type.toString (Type.suggest found)))
                                (TObject
                                    { data
                                        | fields =
                                            data.fields
                                                ++ [ ( fieldName
                                                     , { type_ = Type.suggest found
                                                       , required = True
                                                       , nullable = False
                                                       }
                                                     )
                                                   ]
                                    }
                                )

                        (Type.WrongFieldType fieldName { found }) :: _ ->
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
                                                        , { field | type_ = Type.suggest found }
                                                        )
                                                    )
                                    }
                                )

                _ ->
                    mismatch Nothing (Type.suggest jsHead)


cut : Json -> Json
cut j =
    case j of
        List l ->
            List (List.map cut l)

        String s ->
            String (String.Extra.ellipsis 30 s)

        Object fields ->
            Object (Dict.map (always cut) fields)

        Int _ ->
            j

        Float _ ->
            j

        Bool _ ->
            j

        Null ->
            j


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
                , input = Json.Encode.encode 2 (Json.encode json)
              }
            , Cmd.none
            )

        Save ->
            ( model, File.Download.string "openapi-schema-types.json" "application/json" (Codec.encodeToString 2 typesCodec model.types) )

        Load ->
            ( model, File.Select.file [ "application/json" ] SelectedFile )

        SelectedFile file ->
            ( model, File.toString file |> Task.perform ReadFile )

        ReadFile file ->
            case Codec.decodeString typesCodec file of
                Err _ ->
                    ( model, Cmd.none )

                Ok types ->
                    ( { model | types = types }, Cmd.none )


typesCodec : Codec (Dict String Type)
typesCodec =
    Codec.dict Type.codec


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
