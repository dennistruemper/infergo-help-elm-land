module Components.Purchase exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Shared.Model exposing (Purchase)


view : Purchase -> Html msg
view purchase =
    let
        unitString : String
        unitString =
            if purchase.purchased_amount == 1 then
                String.fromInt purchase.purchased_amount ++ " Unit"

            else
                String.fromInt purchase.purchased_amount ++ " Units"
    in
    Html.div [ class "grid" ]
        [ Html.p [] [ Html.text purchase.product_description ]
        , Html.p [] [ Html.text purchase.purchased_date ]
        , Html.p [] [ Html.text unitString ]
        , Html.p [] [ Html.text (viewPrice purchase.price) ]
        , Html.p [] [ Html.text (String.fromInt purchase.interval_to_previous) ]
        ]


viewPrice : Int -> String
viewPrice price =
    let
        dividedPrice : Int -> Float
        dividedPrice i =
            case String.toFloat (String.fromInt i) of
                Just f ->
                    f / 100

                Nothing ->
                    0.0
    in
    String.fromFloat (dividedPrice price)
