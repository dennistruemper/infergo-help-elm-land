module Components.TrackedItemDennis exposing (view)

import Components.Purchase
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onBlur, onClick, onFocus)
import Shared.Model



-- VIEW


view : { purchases : List Shared.Model.Purchase, isExpanded : Bool, name : String, onFocus : msg, onClick : msg } -> Html msg
view settings =
    let
        pHeaderClass : String
        pHeaderClass =
            "cell has-text-weight-medium"

        firstPurchase : Html msg
        firstPurchase =
            case List.head settings.purchases of
                Just p ->
                    Components.Purchase.view p

                Nothing ->
                    Html.div [] []

        allPurchases : Html msg
        allPurchases =
            Html.div [] (List.map Components.Purchase.view settings.purchases)

        purchases : Html msg
        purchases =
            if List.isEmpty settings.purchases then
                Html.div [ class "is-flex is-justify-content-space-around" ] [ Html.text "No purchases yet!" ]

            else
                Html.div [ class "box" ]
                    [ Html.div [ class "grid" ]
                        [ Html.p [ class pHeaderClass ] [ Html.text "Description" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Date" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Amount" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Price" ]
                        , Html.p [ class pHeaderClass ] [ Html.text "Interval to previous" ]
                        ]
                    , if settings.isExpanded then
                        allPurchases

                      else
                        firstPurchase
                    ]
    in
    Html.div [ class "box pl-6 pr-6", onFocus settings.onFocus, onClick settings.onClick ]
        [ Html.div [ class "is-full mb-2" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text settings.name ] ]
        , purchases
        ]
