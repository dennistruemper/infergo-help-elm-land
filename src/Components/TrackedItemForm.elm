module Components.TrackedItemForm exposing (view)

import Components.Input
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Shared.Model exposing (Purchase, TrackedItem)


view : Html msg
view =
    let
        divClass : String
        divClass =
            "is-flex is-flex-direction-row is-justify-content-space-between"

        pClass : String
        pClass =
            "has-text-weight-medium"
    in
    Html.div [ class "box pl-6 pr-6" ]
        [ Html.span [ class "is-full" ]
            [ Html.h5 [ class "title is-5" ]
                [ Html.input [ class "input", placeholder "Name", type_ "text" ] [] ]
            , Html.h6 [ class "subtitle is-6 is-italic has-text-weight-medium" ]
                [ Html.input [ class "input", placeholder "Description", type_ "text" ] [] ]
            , Html.div [ class divClass ]
                [ Html.p [ class pClass ] [ Html.text "Purchased date " ]
                , Html.p [] [ Html.input [ class "input", placeholder "Date" ] [] ]
                ]
            , Html.div [ class divClass ]
                [ Html.p [ class pClass ] [ Html.text "Amount purchased" ]
                , Html.p [] [ Html.input [ class "input", placeholder "Amount" ] [] ]
                ]
            , Html.div [ class divClass ]
                [ Html.p [ class pClass ] [ Html.text "Price paid" ]
                , Html.p [] [ Html.input [ class "input", placeholder "Price" ] [] ]
                ]
            ]
        ]
