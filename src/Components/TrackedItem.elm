module Components.TrackedItem exposing (Model, Msg, init, new, update, view, withIsExpanded)

import Components.Purchase
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onBlur, onFocus)
import Shared.Model



-- SETTINGS


type TrackedItem trackedItem msg
    = Settings
        { model : Model trackedItem
        , toMsg : Msg trackedItem msg -> msg
        , name : String
        , purchases : List Shared.Model.Purchase
        , isExpanded : Bool
        }


new :
    { model : Model trackedItem
    , toMsg : Msg trackedItem msg -> msg
    , trackedItem : Shared.Model.TrackedItem

    --, name : String
    --, purchases : List Purchase
    }
    -> TrackedItem trackedItem msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , name = props.trackedItem.name
        , purchases = props.trackedItem.purchases
        , isExpanded = False
        }



-- MODIFIERS


withIsExpanded : TrackedItem trackedItem msg -> TrackedItem trackedItem msg
withIsExpanded (Settings settings) =
    Settings { settings | isExpanded = True }



-- MODEL


type Model trackedItem
    = Model
        { isExpanded : Bool
        , name : String
        , purchases : List Shared.Model.Purchase
        }


init : { trackedItem : Shared.Model.TrackedItem } -> Model trackedItem
init props =
    Model
        { isExpanded = False
        , name = props.trackedItem.name
        , purchases = props.trackedItem.purchases
        }



-- UPDATE


type Msg trackedItem msg
    = FocusedTrackedItem
    | BlurredTrackedItem


update :
    { msg : Msg trackedItem msg
    , model : Model trackedItem
    , toModel : Model trackedItem -> model
    , toMsg : Msg trackedItem msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model trackedItem, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            FocusedTrackedItem ->
                ( Model { model | isExpanded = True }, Effect.none )

            BlurredTrackedItem ->
                ( Model { model | isExpanded = False }, Effect.none )



-- VIEW


view : TrackedItem msg trackedItem -> Html msg
view (Settings settings) =
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
    Html.div [ class "box pl-6 pr-6" ]
        --, onFocus (settings.toMsg FocusedTrackedItem) ]
        [ Html.div [ class "is-full mb-2" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text settings.name ] ]
        , purchases
        ]
