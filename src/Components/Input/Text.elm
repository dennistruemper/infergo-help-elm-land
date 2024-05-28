module Components.Input.Text exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput)


view :
    { label : String
    , placeholder : String
    , onInput : String -> msg
    }
    -> Html msg
view props =
    Html.div [ class "flex flex-col p-2" ]
        [ Html.span [ class "p-1" ] [ Html.text props.label ]
        , Html.input
            [ type_ "text"
            , class "input p-1"
            , placeholder props.placeholder
            , onInput props.onInput
            ]
            []
        ]
