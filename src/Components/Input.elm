module Components.Input exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)


view :
    { label : String
    , placeholder : String
    , inputType : String
    , onInput : String -> msg
    }
    -> Html msg
view props =
    Html.div [ class "field pl-6 pr-6" ]
        [ Html.label [ class "label" ] [ Html.text props.label ]
        , Html.div [ class "control" ]
            [ Html.input
                [ type_ props.inputType
                , class "input"
                , placeholder props.placeholder
                , onInput props.onInput
                ]
                []
            ]
        ]
