module Components.TrackedItem exposing (view)

import Components.Purchase
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder)
import Shared.Model exposing (Purchase, TrackedItem)


view : TrackedItem -> Html msg
view t =
    let
        pHeaderClass : String
        pHeaderClass =
            "cell has-text-weight-medium"

        purchases : Html msg
        purchases =
            if List.isEmpty t.purchases then
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
                    , Html.div [] (List.map Components.Purchase.view t.purchases)
                    ]
    in
    Html.div [ class "box pl-6 pr-6" ]
        [ Html.div [ class "is-full mb-2" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text t.name ] ]
        , purchases
        ]
