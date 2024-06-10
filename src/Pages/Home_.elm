module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.Input.Text
import Components.TrackedItem
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Purchase, TrackedItem)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { trackedItems : Api.Data (List TrackedItem)
    , newItemName : String
    , newDescription : String
    , newPurchasedDate : String
    , newPurchasedAmount : Int
    , newPrice : Int
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , newItemName = ""
      , newDescription = ""
      , newPurchasedDate = ""
      , newPurchasedAmount = 0
      , newPrice = 0
      }
    , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
    )



-- UPDATE


type Msg
    = NewItemNameUpdated String
    | NewDescriptionUpdated String
    | NewPurchasedDateUpdated String
    | NewPurchasedAmountUpdated String
    | NewPriceUpdated String
    | NewTrackedItemSubmitted
    | TrackedItemCreated (Result Http.Error String)
    | TrackedItemApiResponded (Result Http.Error (List TrackedItem))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NewItemNameUpdated s ->
            ( { model | newItemName = s }
            , Effect.none
            )

        NewDescriptionUpdated s ->
            ( { model | newDescription = s }
            , Effect.none
            )

        NewPurchasedDateUpdated s ->
            ( { model | newPurchasedDate = s }
            , Effect.none
            )

        NewPurchasedAmountUpdated s ->
            ( { model | newPurchasedAmount = Maybe.withDefault 0 (String.toInt s) }
            , Effect.none
            )

        NewPriceUpdated s ->
            ( { model | newPrice = Maybe.withDefault 0 (String.toInt s) }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( model
            , Api.TrackedItem.create
                { onResponse = TrackedItemCreated
                , name = model.newItemName
                , description = model.newDescription
                , purchase =
                    case model.newPurchasedDate of
                        _ ->
                            Just
                                (Shared.Model.Purchase
                                    model.newPurchasedDate
                                    model.newPurchasedAmount
                                    model.newPrice
                                    0
                                )
                }
            )

        TrackedItemApiResponded (Ok listOfTrackedItems) ->
            ( { model | trackedItems = Api.Success listOfTrackedItems }
            , Effect.none
            )

        TrackedItemApiResponded (Err httpError) ->
            ( { model | trackedItems = Api.Failure httpError }
            , Effect.none
            )

        TrackedItemCreated (Ok s) ->
            ( model
            , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
            )

        TrackedItemCreated (Err httpError) ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Tracked Items"
    , body =
        [ Html.div
            [ class "w-auto" ]
            [ Html.div [ class "grid grid-cols-2 h-dvh" ]
                [ Html.div [ class "flex place-content-around" ]
                    [ case model.trackedItems of
                        Api.Loading ->
                            Html.text "Loading tracked items..."

                        Api.Success trackedItems ->
                            viewTrackedItems trackedItems

                        Api.Failure httpError ->
                            Html.text "Something went wrong retrieving the tracked items"
                    ]
                , viewCreateTrackedItemForm
                ]
            ]
        ]
    }


viewTrackedItems : List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems ts =
    Html.div
        [ class "flex flex-col content-center place-content-center" ]
        [ Html.h1 [] [ Html.text "Overview" ]
        , Html.div [] (List.map Components.TrackedItem.view ts)
        ]


viewCreateTrackedItemForm : Html Msg
viewCreateTrackedItemForm =
    Html.div
        [ class "flex content-center place-content-around" ]
        [ Html.div [ class "flex-col content-center" ]
            [ Html.h1 [ class "h1" ] [ Html.text "Create new tracked item" ]
            , Components.Input.Text.view
                { label = "Name"
                , placeholder = "Name"
                , onInput = NewItemNameUpdated
                }
            , Components.Input.Text.view
                { label = "Description"
                , placeholder = "Description"
                , onInput = NewDescriptionUpdated
                }
            , Components.Input.Text.view
                { label = "Purchased Date"
                , placeholder = "yyyy-mm-dd"
                , onInput = NewPurchasedDateUpdated
                }
            , Components.Input.Text.view
                { label = "Purchased Amount"
                , placeholder = "1"
                , onInput = NewPurchasedAmountUpdated
                }
            , Components.Input.Text.view
                { label = "Price"
                , placeholder = "1795"
                , onInput = NewPriceUpdated
                }
            , Html.button [ class "border button", onClick NewTrackedItemSubmitted ] [ Html.text "Submit" ]
            ]
        ]
