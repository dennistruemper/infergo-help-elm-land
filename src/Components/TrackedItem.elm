module Components.TrackedItem exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Shared.Model exposing (Purchase, TrackedItem)


view : TrackedItem -> Html msg
view t =
    Html.li [ class "border flex" ]
        [ Html.span [ style "width" "100%" ]
            [ Html.text ("Name: " ++ t.name ++ "| Description: " ++ t.product_description)
            ]
        ]
