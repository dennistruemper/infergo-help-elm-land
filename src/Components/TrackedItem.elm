module Components.TrackedItem exposing (Model, Msg, TrackedItem, init, new, update, view, withIsExpanded)

import Components.Purchase
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onBlur, onClick, onFocus)
import Shared.Model



-- SETTINGS


type TrackedItem msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , name : String
        , purchases : List Shared.Model.Purchase
        , isExpanded : Bool
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , name : String
    , purchases : List Shared.Model.Purchase
    }
    -> TrackedItem msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , name = props.name
        , purchases = props.purchases
        , isExpanded = False
        }



-- MODIFIERS


withIsExpanded : TrackedItem msg -> TrackedItem msg
withIsExpanded (Settings settings) =
    Settings { settings | isExpanded = True }



-- MODEL


type Model
    = Model
        { isExpanded : Bool
        , name : String
        , purchases : List Shared.Model.Purchase
        }


init : { name : String, purchases : List Shared.Model.Purchase } -> Model
init props =
    Model
        { isExpanded = False
        , name = props.name
        , purchases = props.purchases
        }



-- UPDATE


type Msg
    = FocusedTrackedItem
    | BlurredTrackedItem


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
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


view : TrackedItem msg -> Html msg
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
    Html.div [ class "box pl-6 pr-6", onFocus (settings.toMsg FocusedTrackedItem), onClick (settings.toMsg FocusedTrackedItem) ]
        [ Html.div [ class "is-full mb-2" ]
            [ Html.h5 [ class "title is-5" ] [ Html.text settings.name ] ]
        , purchases
        ]
