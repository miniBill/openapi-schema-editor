module Theme exposing (deleteButton, extractTypeButton, goToButton, select)

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import List.Extra


select : List (Attribute value) -> List ( String, value ) -> value -> Html value
select attrs selectOptions currentValue =
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
            (Html.Events.onInput
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
                :: attrs
            )


deleteButton : msg -> Html msg
deleteButton msg =
    Html.button
        [ Html.Events.onClick msg
        , Html.Attributes.title "Delete"
        ]
        [ Html.text "🗑️" ]


extractTypeButton : msg -> Html msg
extractTypeButton msg =
    Html.button
        [ Html.Events.onClick msg
        , Html.Attributes.title "Extract type to top level"
        ]
        [ Html.text "💥" ]


goToButton : msg -> Html msg
goToButton msg =
    Html.button
        [ Html.Events.onClick msg
        , Html.Attributes.title "Go to definition"
        ]
        [ Html.text "➡️" ]
