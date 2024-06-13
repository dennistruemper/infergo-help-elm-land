module Components.TrackedItem exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, placeholder)
import Shared.Model exposing (Purchase, TrackedItem)


view : TrackedItem -> Html msg
view t =
    let
        purchases : Html msg
        purchases =
            if List.isEmpty t.purchases then
                Html.div [ class "is-flex is-justify-content-space-around" ] [ Html.text "No purchases yet!" ]

            else
                Html.div [ class "box" ]
                    [ Html.div [ class "grid" ]
                        [ Html.p [ class pHeaderClass ] [ Html.text "Date" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Amount" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Price" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Interval to previous" ]
                        ]
                    , Html.div [] (List.map viewPurchase t.purchases)
                    ]

        pHeaderClass : String
        pHeaderClass =
            "cell has-text-weight-medium"
    in
    Html.div [ class "box pl-6 pr-6" ]
        [ Html.div [ class "is-full mb-2" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text t.name ]
            , Html.h6 [ class "subtitle is-6 is-italic has-text-weight-medium" ] [ Html.text t.product_description ]
            ]
        , purchases
        ]


viewPurchase : Purchase -> Html msg
viewPurchase purchase =
    let
        divClass : String
        divClass =
            "grid"

        pHeaderClass : String
        pHeaderClass =
            "cell has-text-weight-medium"

        unitString : String
        unitString =
            if purchase.purchased_amount == 1 then
                String.fromInt purchase.purchased_amount ++ " Unit"

            else
                String.fromInt purchase.purchased_amount ++ " Units"
    in
    Html.div [ class divClass ]
        [ Html.p [] [ Html.text purchase.purchased_date ]
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
