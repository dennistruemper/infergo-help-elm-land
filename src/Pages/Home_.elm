module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.Input
import Components.TrackedItem
import Components.TrackedItemForm
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
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
    , noItemsYet : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , newItemName = ""
      , newDescription = ""
      , newPurchasedDate = ""
      , newPurchasedAmount = 0
      , newPrice = 0
      , noItemsYet = True
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
    | ShowCreateForm Bool
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

        ShowCreateForm True ->
            ( { model | noItemsYet = True }
            , Effect.none
            )

        ShowCreateForm False ->
            ( { model | noItemsYet = False }
            , Effect.none
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
            [ class "is-full fixed-grid has-6-cols" ]
            [ Html.div [ class "grid is-flex is-flex-direction-column" ]
                [ Html.div [ class "cell" ] []
                , Html.div [ class "cell is-col-span-4" ]
                    [ case model.trackedItems of
                        Api.Loading ->
                            Html.span [ class "is-size-4 has-text-centered" ] [ Html.text "Loading tracked items..." ]

                        Api.Success trackedItems ->
                            viewTrackedItems model.noItemsYet trackedItems

                        Api.Failure httpError ->
                            Html.span [ class "is-size-4 has-text-centered" ] [ Html.text "Something went wrong retrieving the tracked items" ]
                    ]
                , Html.div [ class "cell" ] []
                , viewCreateTrackedItemForm model.noItemsYet
                ]
            ]
        ]
    }


viewShowCreateFormButton : Html Msg
viewShowCreateFormButton =
    Html.button [ class "button is-success", onClick (ShowCreateForm False) ] [ Html.text "+" ]


viewTrackedItems : Bool -> List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems noItemsYet ts =
    let
        listView : Html Msg
        listView =
            if List.isEmpty ts then
                Html.div [ class "is-flex is-flex-direction-column is-justify-content-center" ]
                    [ Html.h2 [ class "subtitle is-2 has-text-centered" ] [ Html.text "No tracked items yet! Ready to add one?" ]
                    , if noItemsYet == True then
                        viewShowCreateFormButton

                      else
                        Html.div [] []
                    ]

            else
                Html.div [] (List.map Components.TrackedItem.view ts)
    in
    Html.div []
        [ Html.h1 [ class "title is-1 has-text-centered" ] [ Html.text "Tracked Items" ]
        , listView
        ]


viewCreateTrackedItemForm : Bool -> Html Msg
viewCreateTrackedItemForm noItemsYet =
    if noItemsYet == True then
        Html.div [] [ Components.TrackedItemForm.view ]

    else
        Html.div [ class "cell field" ]
            [ Html.h1 [ class "title is-1 has-text-centered" ] [ Html.text "Create new tracked item" ]
            , Components.Input.view
                { label = "Name"
                , placeholder = "Name"
                , inputType = "text"
                , onInput = NewItemNameUpdated
                }
            , Components.Input.view
                { label = "Description"
                , placeholder = "Description"
                , inputType = "text"
                , onInput = NewDescriptionUpdated
                }
            , Components.Input.view
                { label = "Purchased Date"
                , placeholder = "yyyy-mm-dd"
                , inputType = "text"
                , onInput = NewPurchasedDateUpdated
                }
            , Components.Input.view
                { label = "Purchased Amount"
                , placeholder = "1"
                , inputType = "text"
                , onInput = NewPurchasedAmountUpdated
                }
            , Components.Input.view
                { label = "Price"
                , placeholder = "1795"
                , inputType = "text"
                , onInput = NewPriceUpdated
                }
            , Html.div [ class "pl-6 pr-6" ] [ Html.button [ class "button", onClick NewTrackedItemSubmitted ] [ Html.text "Submit" ] ]
            ]
