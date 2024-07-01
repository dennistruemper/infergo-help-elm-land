module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.PurchaseInput
import Components.TrackedItem
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
    , purchaseInput : Components.PurchaseInput.Model
    , newItemName : String
    , newDescription : String
    , newPurchasedDateYear : String
    , newPurchasedDateMonth : String
    , newPurchasedDateDay : String
    , newPurchasedAmount : Int
    , newPrice : Int
    , showCreateForm : Bool
    , addPurchase : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , purchaseInput = Components.PurchaseInput.init
      , newItemName = ""
      , newDescription = ""
      , newPurchasedDateYear = ""
      , newPurchasedDateMonth = ""
      , newPurchasedDateDay = ""
      , newPurchasedAmount = 0
      , newPrice = 0
      , showCreateForm = False
      , addPurchase = False
      }
    , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
    )



-- UPDATE


type Msg
    = NewItemNameUpdated String
    | NewDescriptionUpdated String
    | NewPurchasedDateYearUpdated String
    | NewPurchasedDateMonthUpdated String
    | NewPurchasedDateDayUpdated String
    | NewPurchasedAmountUpdated String
    | NewPriceUpdated String
    | AddPurchaseClicked
    | NewTrackedItemSubmitted
    | ClearNewTransactionForm
    | ShowCreateForm Bool
    | PurchaseInputCompleted (Components.PurchaseInput.Msg Msg)
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

        NewPurchasedDateYearUpdated year ->
            ( { model | newPurchasedDateYear = year }
            , Effect.none
            )

        NewPurchasedDateMonthUpdated month ->
            ( { model | newPurchasedDateMonth = month }
            , Effect.none
            )

        NewPurchasedDateDayUpdated day ->
            ( { model | newPurchasedDateDay = day }
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

        AddPurchaseClicked ->
            ( { model | addPurchase = True }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( { model | showCreateForm = False }
            , Api.TrackedItem.create
                { onResponse = TrackedItemCreated
                , name = model.newItemName
                , description = model.newDescription
                , purchase =
                    case model.newPrice of
                        _ ->
                            Just
                                (Shared.Model.Purchase
                                    (model.newPurchasedDateYear
                                        ++ "-"
                                        ++ model.newPurchasedDateMonth
                                        ++ "-"
                                        ++ model.newPurchasedDateDay
                                    )
                                    model.newPurchasedAmount
                                    model.newPrice
                                    0
                                )
                }
            )

        ClearNewTransactionForm ->
            ( { model
                | newItemName = ""
                , newDescription = ""
                , newPurchasedDateYear = ""
                , newPurchasedDateMonth = ""
                , newPurchasedDateDay = ""
                , newPurchasedAmount = 0
                , newPrice = 0
                , showCreateForm = False
                , addPurchase = False
              }
            , Effect.none
            )

        ShowCreateForm value ->
            ( { model
                | showCreateForm =
                    if model.showCreateForm == True then
                        False

                    else
                        True
              }
            , Effect.none
            )

        PurchaseInputCompleted innerMsg ->
            Components.PurchaseInput.update
                { msg = innerMsg
                , model = model.purchaseInput
                , toModel = \purchaseInput -> { model | purchaseInput = purchaseInput }
                , toMsg = PurchaseInputCompleted
                }

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
            [ Html.div [ class "grid" ]
                [ Html.div [ class "cell" ] []
                , Html.div [ class "cell is-col-span-4 is-flex is-flex-direction-column" ]
                    [ Html.div []
                        [ case model.trackedItems of
                            Api.Loading ->
                                Html.span [ class "is-size-4 has-text-centered" ] [ Html.text "Loading tracked items..." ]

                            Api.Success trackedItems ->
                                viewTrackedItems model.showCreateForm trackedItems

                            Api.Failure httpError ->
                                Html.span [ class "is-size-4 has-text-centered" ] [ Html.text "Something went wrong retrieving the tracked items" ]
                        ]
                    , viewCreateTrackedItemForm model
                    , Html.div [ class "is-flex is-justify-content-center" ] [ viewShowCreateFormButton ]
                    ]
                , Html.div [ class "cell" ] []
                ]
            ]
        ]
    }


viewTrackedItems : Bool -> List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems showCreateForm ts =
    let
        listView : Html Msg
        listView =
            if List.isEmpty ts then
                Html.div [ class "is-flex is-flex-direction-column is-justify-content-center" ]
                    [ Html.h2 [ class "subtitle is-2 has-text-centered" ] [ Html.text "No tracked items yet! Ready to add one?" ] ]

            else
                Html.div [] (List.map Components.TrackedItem.view ts)
    in
    Html.div []
        [ Html.h1 [ class "title is-1 has-text-centered" ] [ Html.text "Tracked Items" ]
        , listView
        ]


viewShowCreateFormButton : Html Msg
viewShowCreateFormButton =
    Html.button [ class "button is-primary is-medium is-fullwidth", onClick (ShowCreateForm False) ] [ Html.text "New Transaction" ]


viewCreateTrackedItemForm : Model -> Html Msg
viewCreateTrackedItemForm model =
    let
        divClass : String
        divClass =
            "mt-2 is-flex is-flex-direction-row is-justify-content-space-between"

        pClass : String
        pClass =
            "has-text-weight-medium"
    in
    if model.showCreateForm == False then
        Html.div [] []

    else
        Html.div [ class "box pl-6 pr-6" ]
            [ Html.div [ class "is-full" ]
                [ Html.div [ class "field" ]
                    [ Html.label [ class "label" ] [ Html.text "Name" ]
                    , Html.div [ class "control" ] [ Html.input [ class "input", placeholder "Shampoo", type_ "text", onInput NewItemNameUpdated ] [] ]
                    ]
                , Html.div [ class "field" ]
                    [ Html.label [ class "label" ] [ Html.text "Description" ]
                    , Html.div [ class "control" ] [ Html.input [ class "input", placeholder "Reuzel Daily Shampoo", type_ "text", onInput NewDescriptionUpdated ] [] ]
                    ]
                , if model.addPurchase == True then
                    Html.div [ class "field" ]
                        [ Html.label [ class "label" ] [ Html.text "Purchases" ]
                        , Components.PurchaseInput.new
                            { model = model.purchaseInput
                            , toMsg = PurchaseInputCompleted
                            }
                            |> Components.PurchaseInput.view
                        ]

                  else
                    Html.div [] []
                , if model.addPurchase == False then
                    Html.div [ class "field" ]
                        [ Html.button [ class "button is-dark", onClick AddPurchaseClicked ] [ Html.text "Add purchase" ] ]

                  else
                    Html.div [] []
                , Html.div [ class "mt-2 is-flex is-justify-content-space-around" ]
                    [ Html.button [ class "button is-primary", onClick NewTrackedItemSubmitted ] [ Html.text "Save" ]
                    , Html.button [ class "button is-secondary", onClick ClearNewTransactionForm ] [ Html.text "Cancel" ]
                    ]
                ]
            ]
