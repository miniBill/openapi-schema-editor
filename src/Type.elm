module Type exposing
    ( AdditionalProperties(..)
    , Extracted
    , Field
    , ObjectData
    , ObjectMatchProblem(..)
    , StringData
    , StringMatchProblem(..)
    , Type(..)
    , codec
    , editor
    , isValidFor
    , matchesObject
    , suggest
    , toString
    )

import Codec exposing (Codec)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json exposing (Json(..))
import Json.Encode
import List.Extra
import Maybe.Extra
import Parser.Advanced
import Regex exposing (Regex)
import Result.Extra
import Rfc3339
import Theme
import Url


type Type
    = TList Type
    | TString StringData
    | TInteger
    | TNumber
    | TBoolean
    | TNull
    | TObject ObjectData
    | TOneOf (List Type)
    | TRef String


type alias StringData =
    { pattern : Maybe String
    , const : Maybe String
    , format : Maybe String
    }


type alias ObjectData =
    { fields : List ( String, Field )
    , additionalProperties : AdditionalProperties
    }


type alias Field =
    { type_ : Type
    , required : Bool
    , nullable : Bool
    }


type AdditionalProperties
    = AdditionalPropertiesAllowed (Maybe Type)
    | AdditionalPropertiesNotAllowed


toShortString : Type -> String
toShortString t =
    case t of
        TObject _ ->
            "object"

        TList _ ->
            "list"

        TString _ ->
            "string"

        TInteger ->
            "integer"

        TNumber ->
            "number"

        TBoolean ->
            "boolean"

        TNull ->
            "null"

        TOneOf _ ->
            "oneOf"

        TRef _ ->
            "$ref"


toString : Type -> String
toString t =
    case t of
        TObject data ->
            if List.isEmpty data.fields then
                "{}"

            else
                "{ " ++ String.join ", " (List.map fieldToString data.fields) ++ " }"

        TList child ->
            toString child ++ "[]"

        TString data ->
            case data.const of
                Just c ->
                    escape c

                Nothing ->
                    case data.pattern of
                        Just p ->
                            "/" ++ p ++ "/"

                        Nothing ->
                            case data.format of
                                Just f ->
                                    "string:" ++ f

                                Nothing ->
                                    "string"

        TInteger ->
            "integer"

        TNumber ->
            "number"

        TBoolean ->
            "boolean"

        TNull ->
            "null"

        TOneOf alt ->
            alt |> List.map toString |> String.join " | "

        TRef name ->
            "$ref: " ++ name


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


fieldToString : ( String, Field ) -> String
fieldToString ( fieldName, field ) =
    case ( field.required, field.nullable ) of
        ( True, False ) ->
            fieldName ++ ": " ++ toString field.type_

        ( False, False ) ->
            fieldName ++ "?: " ++ toString field.type_

        ( True, True ) ->
            fieldName ++ ": " ++ toString field.type_ ++ "?"

        ( False, True ) ->
            fieldName ++ "?: " ++ toString field.type_ ++ "?"


editor : List String -> Type -> Html Type
editor typeNames t =
    let
        default : Extracted
        default =
            defaultExtracted t

        ( extracted, inline, additional ) =
            case t of
                TObject obj ->
                    objectEditor typeNames obj default

                TList list ->
                    ( { default | list = Just list }
                    , Html.text ""
                    , [ Html.map TList (editor typeNames list) ]
                    )

                TString str ->
                    stringEditor str default

                TInteger ->
                    ( default, Html.text "", [] )

                TNumber ->
                    ( default, Html.text "", [] )

                TBoolean ->
                    ( default, Html.text "", [] )

                TNull ->
                    ( default, Html.text "", [] )

                TRef name ->
                    ( { default | ref = Just name }
                    , Html.text ""
                    , [ Html.label
                            [ Html.Attributes.style "white-space" "no-wrap" ]
                            [ Html.text "$ref: "
                            , Theme.select []
                                (List.map
                                    (\typeName ->
                                        ( if String.isEmpty typeName then
                                            "<root>"

                                          else
                                            typeName
                                        , TRef typeName
                                        )
                                    )
                                    typeNames
                                )
                                t
                            ]
                      ]
                    )

                TOneOf children ->
                    ( { default | oneOf = children }
                    , Html.button
                        [ Html.Attributes.style "grid-column" "1 / span 3"
                        , Html.Events.onClick
                            (TOneOf
                                (children ++ [ TRef "" ])
                            )
                        ]
                        [ Html.text "➕ New alternative" ]
                    , (children
                        |> List.indexedMap
                            (\i child ->
                                [ Html.div [] []
                                , editor typeNames child
                                    |> Html.map
                                        (\newChild ->
                                            TOneOf (List.Extra.setAt i newChild children)
                                        )
                                , Html.button
                                    [ Html.Events.onClick (TOneOf (List.Extra.removeAt i children)) ]
                                    [ Html.text "🗑️" ]
                                ]
                            )
                        |> List.concat
                      )
                        |> Html.div
                            [ Html.Attributes.style "display" "grid"
                            , Html.Attributes.style "gap" "4px"
                            , Html.Attributes.style "grid-template-columns" "4px auto auto"
                            ]
                        |> List.singleton
                    )
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "4px"
        ]
        (Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "4px"
            ]
            [ Theme.select
                [ Html.Attributes.style "flex" "1" ]
                ([ TObject (Maybe.withDefault emptyObject extracted.obj)
                 , TList (Maybe.withDefault TInteger extracted.list)
                 , TOneOf extracted.oneOf
                 , TString
                    (Maybe.withDefault
                        { pattern = Nothing
                        , const = Nothing
                        , format = Nothing
                        }
                        extracted.string
                    )
                 , TInteger
                 , TNumber
                 , TBoolean
                 , TNull
                 , TRef (Maybe.withDefault "" extracted.ref)
                 ]
                    |> List.map (\k -> ( toShortString k, k ))
                )
                t
            , inline
            ]
            :: additional
        )


stringEditor : StringData -> Extracted -> ( Extracted, Html Type, List (Html Type) )
stringEditor str default =
    let
        checkboxedInput : String -> Maybe String -> (Maybe String -> StringData) -> List (Html Type)
        checkboxedInput label value toMsg =
            [ Html.label
                [ Html.Attributes.style "display" "flex"
                ]
                [ Html.span
                    [ Html.Attributes.style "display" "block"
                    , Html.Attributes.style "flex" "1"
                    ]
                    [ Html.text (label ++ " ") ]
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.checked (value /= Nothing)
                    , Html.Events.onCheck
                        (\newValue ->
                            TString
                                (toMsg
                                    (if newValue then
                                        Just ""

                                     else
                                        Nothing
                                    )
                                )
                        )
                    ]
                    []
                ]
            , Html.input
                [ Html.Events.onInput (\newValue -> TString (toMsg (Just newValue)))
                , Html.Attributes.value (Maybe.withDefault "" value)
                ]
                []
            ]
    in
    ( { default | string = Just str }
    , Html.text ""
    , [ Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "auto 1fr"
            ]
            (checkboxedInput "Pattern" str.pattern (\newValue -> { str | pattern = newValue })
                ++ checkboxedInput "Const" str.const (\newValue -> { str | const = newValue })
                ++ checkboxedInput "Format" str.format (\newValue -> { str | format = newValue })
            )
      ]
    )


type alias Extracted =
    { obj : Maybe ObjectData
    , list : Maybe Type
    , string : Maybe StringData
    , oneOf : List Type
    , ref : Maybe String
    }


defaultExtracted : Type -> Extracted
defaultExtracted t =
    { obj = Nothing
    , list = Nothing
    , string = Nothing
    , oneOf = [ t ]
    , ref = Nothing
    }


objectEditor : List String -> ObjectData -> Extracted -> ( Extracted, Html Type, List (Html Type) )
objectEditor typeNames obj default =
    let
        fieldsViews : List (Html Type)
        fieldsViews =
            List.indexedMap viewField obj.fields
                |> List.concat

        viewField : Int -> ( String, Field ) -> List (Html Type)
        viewField i ( fieldName, field ) =
            [ Html.input
                [ Html.Attributes.value fieldName
                , Html.Events.onInput
                    (\newFieldName ->
                        TObject
                            { obj
                                | fields =
                                    List.Extra.setAt i
                                        ( newFieldName, field )
                                        obj.fields
                            }
                    )
                ]
                []
            , editor typeNames field.type_
                |> Html.map
                    (\newType ->
                        TObject
                            { obj
                                | fields =
                                    List.Extra.setAt i
                                        ( fieldName, { field | type_ = newType } )
                                        obj.fields
                            }
                    )
            , Html.button
                [ Html.Events.onClick
                    (TObject
                        { obj
                            | fields =
                                List.Extra.removeAt i obj.fields
                        }
                    )
                ]
                [ Html.text "🗑️" ]
            ]

        newFieldView : Html Type
        newFieldView =
            Html.button
                [ Html.Attributes.style "grid-column" "1 / span 3"
                , Html.Events.onClick
                    (TObject
                        { obj
                            | fields = obj.fields ++ [ ( "", { type_ = TNull, required = True, nullable = False } ) ]
                        }
                    )
                ]
                [ Html.text "➕ New field" ]

        additionalPropertiesViews : List (Html Type)
        additionalPropertiesViews =
            [ Theme.select []
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
                (TObject obj)
            ]
    in
    ( { default | obj = Just obj }
    , newFieldView
    , Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "gap" "4px"
        , Html.Attributes.style "grid-template-columns" "auto auto auto"
        ]
        fieldsViews
        :: additionalPropertiesViews
    )


emptyObject : ObjectData
emptyObject =
    { fields = []
    , additionalProperties = AdditionalPropertiesNotAllowed
    }


union : Type -> Type -> Type
union l r =
    case ( l, r ) of
        ( TOneOf lo, TOneOf ro ) ->
            TOneOf (List.Extra.unique (lo ++ ro))

        ( TOneOf lo, _ ) ->
            TOneOf (List.Extra.unique (r :: lo))

        ( _, TOneOf ro ) ->
            TOneOf (List.Extra.unique (l :: ro))

        _ ->
            if l == r then
                l

            else
                TOneOf [ l, r ]


isValidFor : Type -> Json -> Bool
isValidFor t j =
    case ( j, t ) of
        ( Int _, TInteger ) ->
            True

        ( List children, TList c ) ->
            List.all (\child -> isValidFor c child) children

        ( Float _, TNumber ) ->
            True

        ( String s, TString str ) ->
            case matchesString str s of
                Ok () ->
                    True

                Err _ ->
                    False

        ( Bool _, TBoolean ) ->
            True

        ( Null, TNull ) ->
            True

        ( Object o, TObject obj ) ->
            List.isEmpty (matchesObject obj o)

        ( _, TRef _ ) ->
            True

        _ ->
            False


type StringMatchProblem
    = IsNotConst String
    | DoesNotMatchPattern
    | DoesNotMatchFormat String
    | UnknownFormat String


matchesString : StringData -> String -> Result StringMatchProblem ()
matchesString { pattern, const, format } s =
    case ( const, pattern, format ) of
        ( Just c, _, _ ) ->
            if c == s then
                Ok ()

            else
                Err (IsNotConst c)

        ( Nothing, Just p, _ ) ->
            let
                regex : Regex
                regex =
                    Regex.fromString p
                        |> Maybe.withDefault Regex.never
            in
            if Regex.contains regex s then
                Ok ()

            else
                Err DoesNotMatchPattern

        ( Nothing, Nothing, Just f ) ->
            case Dict.get f formats of
                Just isMatch ->
                    if isMatch s then
                        Ok ()

                    else
                        Err (DoesNotMatchFormat f)

                Nothing ->
                    Err (UnknownFormat f)

        ( Nothing, Nothing, Nothing ) ->
            Ok ()


formats : Dict String (String -> Bool)
formats =
    [ ( "date-time", \s -> Parser.Advanced.run Rfc3339.dateTimeOffsetParser s |> Result.Extra.isOk )
    , ( "uri", \s -> Url.fromString s |> Maybe.Extra.isJust )
    ]
        |> Dict.fromList


type ObjectMatchProblem
    = UnexpectedField String { found : Json }
    | WrongFieldType String { expected : Type, found : Json }


matchesObject : ObjectData -> Dict String Json -> List ObjectMatchProblem
matchesObject { fields, additionalProperties } v =
    let
        fieldsDict : Dict String Field
        fieldsDict =
            Dict.fromList fields
    in
    v
        |> Dict.toList
        |> List.concatMap
            (\( fieldName, fieldValue ) ->
                case Dict.get fieldName fieldsDict of
                    Just field ->
                        if isValidFor field.type_ fieldValue then
                            []

                        else if fieldValue == Null && field.nullable then
                            []

                        else
                            [ WrongFieldType fieldName { expected = field.type_, found = fieldValue } ]

                    Nothing ->
                        case additionalProperties of
                            AdditionalPropertiesNotAllowed ->
                                [ UnexpectedField fieldName { found = fieldValue } ]

                            AdditionalPropertiesAllowed Nothing ->
                                []

                            AdditionalPropertiesAllowed (Just additionalType) ->
                                if isValidFor additionalType fieldValue then
                                    []

                                else
                                    [ WrongFieldType fieldName { expected = additionalType, found = fieldValue } ]
            )


suggest : Json -> Type
suggest j =
    case j of
        List [] ->
            TList TNull

        List (h :: t) ->
            t
                |> List.foldl
                    (\e a -> union (suggest e) a)
                    (suggest h)
                |> TList

        Int _ ->
            TInteger

        Float _ ->
            TNumber

        String s ->
            case Dict.Extra.find (\_ isMatch -> isMatch s) formats of
                Just ( formatName, _ ) ->
                    TString { pattern = Nothing, const = Nothing, format = Just formatName }

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
                                        ( "type"
                                        , { type_ = TString { format = Nothing, pattern = Nothing, const = Just s }
                                          , required = True
                                          , nullable = False
                                          }
                                        )

                                    _ ->
                                        ( fieldName
                                        , { type_ = suggest fieldValue
                                          , required = True
                                          , nullable = False
                                          }
                                        )
                            )
                , additionalProperties = AdditionalPropertiesNotAllowed
                }


codec : Codec Type
codec =
    Codec.recursive
        (\go ->
            Codec.custom
                (\vList vString vInteger vNumber vBoolean vNull vObject vOneOf vRef value ->
                    case value of
                        TList c ->
                            vList c

                        TString c ->
                            vString c

                        TInteger ->
                            vInteger

                        TNumber ->
                            vNumber

                        TBoolean ->
                            vBoolean

                        TNull ->
                            vNull

                        TObject c ->
                            vObject c

                        TOneOf c ->
                            vOneOf c

                        TRef c ->
                            vRef c
                )
                |> Codec.variant1 "TList" TList go
                |> Codec.variant1 "TString" TString stringDataCodec
                |> Codec.variant0 "TInteger" TInteger
                |> Codec.variant0 "TNumber" TNumber
                |> Codec.variant0 "TBoolean" TBoolean
                |> Codec.variant0 "TNull" TNull
                |> Codec.variant1 "TObject" TObject objectDataCodec
                |> Codec.variant1 "TOneOf" TOneOf (Codec.list go)
                |> Codec.variant1 "TRef" TRef Codec.string
                |> Codec.buildCustom
        )


stringDataCodec : Codec StringData
stringDataCodec =
    Codec.object StringData
        |> Codec.optionalField "pattern" .pattern Codec.string
        |> Codec.optionalField "const" .const Codec.string
        |> Codec.optionalField "format" .format Codec.string
        |> Codec.buildObject


objectDataCodec : Codec ObjectData
objectDataCodec =
    Codec.object ObjectData
        |> Codec.field "fields" .fields (Codec.list (Codec.tuple Codec.string fieldCodec))
        |> Codec.field "additionalProperties" .additionalProperties additionalPropertiesCodec
        |> Codec.buildObject


fieldCodec : Codec Field
fieldCodec =
    Codec.object Field
        |> Codec.field "type_" .type_ (Codec.lazy (\_ -> codec))
        |> Codec.field "required" .required Codec.bool
        |> Codec.field "nullable" .nullable Codec.bool
        |> Codec.buildObject


additionalPropertiesCodec : Codec AdditionalProperties
additionalPropertiesCodec =
    Codec.custom
        (\additionalPropertiesAllowedEncoder additionalPropertiesNotAllowedEncoder value ->
            case value of
                AdditionalPropertiesAllowed arg0 ->
                    additionalPropertiesAllowedEncoder arg0

                AdditionalPropertiesNotAllowed ->
                    additionalPropertiesNotAllowedEncoder
        )
        |> Codec.variant1 "AdditionalPropertiesAllowed" AdditionalPropertiesAllowed (Codec.nullable (Codec.lazy (\_ -> codec)))
        |> Codec.variant0 "AdditionalPropertiesNotAllowed" AdditionalPropertiesNotAllowed
        |> Codec.buildCustom
