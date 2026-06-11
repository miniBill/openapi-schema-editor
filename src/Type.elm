module Type exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json exposing (Json(..))
import List.Extra
import Parser.Advanced
import Regex exposing (Regex)
import Rfc3339
import Theme


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


toString : Type -> String
toString t =
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

        TRef name ->
            "$ref: " ++ name


editor : Type -> Html Type
editor t =
    let
        default : Extracted
        default =
            defaultExtracted t

        ( extracted, additional ) =
            case t of
                TObject obj ->
                    objectEditor obj default

                TList list ->
                    ( { default | list = Just list }
                    , [ Html.map TList (editor list) ]
                    )

                TString str ->
                    stringEditor str default

                TInteger ->
                    ( default, [] )

                TNumber ->
                    ( default, [] )

                TBoolean ->
                    ( default, [] )

                TNull ->
                    ( default, [] )

                TRef name ->
                    ( { default | ref = Just name }
                    , [ Html.label []
                            [ Html.text "$ref: "
                            , Html.input
                                [ Html.Attributes.value name
                                , Html.Events.onInput TRef
                                ]
                                []
                            ]
                      ]
                    )

                TOneOf children ->
                    ( { default | oneOf = children }
                    , children
                        |> List.indexedMap
                            (\i child ->
                                [ Html.div [] []
                                , editor child
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
        (Theme.select
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
                |> List.map (\k -> ( toString k, k ))
            )
            t
            :: additional
        )


stringEditor : StringData -> Extracted -> ( Extracted, List (Html Type) )
stringEditor str default =
    ( { default | string = Just str }
    , [ Html.div []
            [ Html.label []
                [ Html.text "Pattern "
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.checked (str.pattern /= Nothing)
                    , Html.Events.onCheck
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
                ]
            , Html.input
                [ Html.Events.onInput
                    (\newPattern ->
                        TString
                            { str
                                | pattern = Just newPattern
                            }
                    )
                , Html.Attributes.value (Maybe.withDefault "" str.pattern)
                ]
                []
            ]
      , Html.div []
            [ Html.label []
                [ Html.text "Const "
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.checked (str.const /= Nothing)
                    , Html.Events.onCheck
                        (\newValue ->
                            TString
                                { str
                                    | const =
                                        if newValue then
                                            Just ""

                                        else
                                            Nothing
                                }
                        )
                    ]
                    []
                ]
            , Html.input
                [ Html.Events.onInput
                    (\newConst ->
                        TString
                            { str
                                | const = Just newConst
                            }
                    )
                , Html.Attributes.value (Maybe.withDefault "" str.const)
                ]
                []
            ]
      , Html.div []
            [ Html.label []
                [ Html.text "Format "
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.checked (str.format /= Nothing)
                    , Html.Events.onCheck
                        (\newValue ->
                            TString
                                { str
                                    | format =
                                        if newValue then
                                            Just ""

                                        else
                                            Nothing
                                }
                        )
                    ]
                    []
                ]
            , Html.input
                [ Html.Events.onInput
                    (\newFormat ->
                        TString
                            { str
                                | format = Just newFormat
                            }
                    )
                , Html.Attributes.value (Maybe.withDefault "" str.format)
                ]
                []
            ]
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


objectEditor : ObjectData -> Extracted -> ( Extracted, List (Html Type) )
objectEditor obj default =
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
            , editor field.type_
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
            Html.input
                [ Html.Attributes.style "grid-column" "1 / span 3"
                , Html.Attributes.value ""
                , Html.Events.onInput
                    (\newFieldName ->
                        TObject
                            { obj
                                | fields = obj.fields ++ [ ( newFieldName, { type_ = TNull, required = True, nullable = False } ) ]
                            }
                    )
                ]
                []

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
                (TObject obj)
            ]
    in
    ( { default | obj = Just obj }
    , Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "gap" "4px"
        , Html.Attributes.style "grid-template-columns" "auto auto auto"
        ]
        fieldsViews
        :: newFieldView
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
            case f of
                "date-time" ->
                    case Parser.Advanced.run Rfc3339.dateTimeOffsetParser s of
                        Ok _ ->
                            Ok ()

                        Err _ ->
                            Err (DoesNotMatchFormat f)

                _ ->
                    Err (UnknownFormat f)

        ( Nothing, Nothing, Nothing ) ->
            Ok ()


type alias ObjectMatchProblem =
    { fieldName : String
    , problem : FieldMatchProblem
    }


type FieldMatchProblem
    = UnexpectedField
    | WrongFieldType


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
                            [ { fieldName = fieldName, problem = WrongFieldType } ]

                    Nothing ->
                        case additionalProperties of
                            AdditionalPropertiesNotAllowed ->
                                [ { fieldName = fieldName, problem = UnexpectedField } ]

                            AdditionalPropertiesAllowed Nothing ->
                                []

                            AdditionalPropertiesAllowed (Just additionalType) ->
                                if isValidFor additionalType fieldValue then
                                    []

                                else
                                    [ { fieldName = fieldName, problem = WrongFieldType } ]
            )
