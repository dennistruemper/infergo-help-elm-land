module Components.TrackedItem exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Shared.Model exposing (Purchase, TrackedItem)


view : TrackedItem -> Html msg
view t =
    Html.div [ class "flex min-h-4" ]
        [ Html.span [ class "w-auto" ]
            [ Html.p [] [ Html.text ("Name: " ++ t.name ++ "| Description: " ++ t.product_description) ] ]
        , Html.p [] (List.map viewPurchase t.purchases)
        ]


viewPurchase : Purchase -> Html msg
viewPurchase purchase =
    Html.div [ class "w-auto" ]
        [ Html.text
            ("Purchased date: "
                ++ purchase.purchased_date
                ++ " | Purchased amount: "
                ++ String.fromInt purchase.purchased_amount
                ++ " | Price: "
                ++ String.fromInt purchase.price
                ++ " | Interval to previous: "
                ++ String.fromInt purchase.interval_to_previous
            )
        ]
