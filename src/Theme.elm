module Theme exposing (select)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra


select : List ( String, value ) -> value -> Html value
select selectOptions currentValue =
    selectOptions
        |> List.map
            (\( label, optionValue ) ->
                Html.option
                    [ Html.Attributes.selected (optionValue == currentValue)
                    , Html.Attributes.value label
                    ]
                    [ Html.text label ]
            )
        |> Html.select
            [ Html.Events.onInput
                (\key ->
                    selectOptions
                        |> List.Extra.findMap
                            (\( k, v ) ->
                                if k == key then
                                    Just v

                                else
                                    Nothing
                            )
                        |> Maybe.withDefault currentValue
                )
            ]
