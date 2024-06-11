module Components.TrackedItem exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Shared.Model exposing (Purchase, TrackedItem)


view : TrackedItem -> Html msg
view t =
    Html.div [ class "pl-6 pr-6" ]
        [ Html.span [ class "is-full" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text t.name ]
            , Html.h6 [ class "subtitle is-6 is-italic has-text-weight-medium" ] [ Html.text t.product_description ]
            ]
        , Html.span []
            [ Html.p [ class "pl-4" ] [ Html.text "Purchases" ]
            , Html.div [] (List.map viewPurchase t.purchases)
            ]
        ]


viewPurchase : Purchase -> Html msg
viewPurchase purchase =
    let
        divClass : String
        divClass =
            "is-flex is-flex-direction-row is-justify-content-space-between"

        pClass : String
        pClass =
            "has-text-weight-medium"
    in
    Html.div [ class "box" ]
        [ Html.div [ class divClass ]
            [ Html.p [ class pClass ] [ Html.text "Purchased date " ]
            , Html.p [] [ Html.text purchase.purchased_date ]
            ]
        , Html.div [ class divClass ]
            [ Html.p [ class pClass ] [ Html.text "Amount purchased" ]
            , Html.p [] [ Html.text (String.fromInt purchase.purchased_amount) ]
            ]
        , Html.div [ class divClass ]
            [ Html.p [ class pClass ] [ Html.text "Price paid" ]
            , Html.p [] [ Html.text (viewPrice purchase.price) ]
            ]
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
