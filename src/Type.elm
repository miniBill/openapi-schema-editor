module Type exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Theme


type Type
    = TList Type
    | TString { pattern : Maybe String }
    | TInteger
    | TNumber
    | TBoolean
    | TNull
    | TObject
        { fields : List ( String, Field )
        , additionalProperties : AdditionalProperties
        }
    | TOneOf (List Type)


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
                      ]
                    )

                TInteger ->
                    ( default, [] )

                TNumber ->
                    ( default, [] )

                TBoolean ->
                    ( default, [] )

                TNull ->
                    ( default, [] )

                TOneOf children ->
                    ( { default | oneOf = children }
                    , children
                        |> List.indexedMap
                            (\i child ->
                                editor child
                                    |> Html.map
                                        (\newChild ->
                                            TOneOf (List.Extra.setAt i newChild children)
                                        )
                            )
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
             , TString (Maybe.withDefault { pattern = Nothing } extracted.string)
             , TInteger
             , TNumber
             , TBoolean
             , TNull
             ]
                |> List.map (\k -> ( toString k, k ))
            )
            t
            :: additional
        )


type alias Extracted =
    { obj :
        Maybe
            { fields : List ( String, Field )
            , additionalProperties : AdditionalProperties
            }
    , list : Maybe Type
    , string : Maybe { pattern : Maybe String }
    , oneOf : List Type
    }


defaultExtracted : Type -> Extracted
defaultExtracted t =
    { obj = Nothing
    , list = Nothing
    , string = Nothing
    , oneOf = [ t ]
    }


objectEditor :
    { fields : List ( String, Field )
    , additionalProperties : AdditionalProperties
    }
    -> Extracted
    -> ( Extracted, List (Html Type) )
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
            ]

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
        , Html.Attributes.style "grid-template-columns" "auto auto"
        ]
        fieldsViews
        :: additionalPropertiesViews
    )


emptyObject :
    { fields : List ( String, Field )
    , additionalProperties : AdditionalProperties
    }
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
