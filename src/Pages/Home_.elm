module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.PurchaseInput
import Components.TrackedItem
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (NewPurchase, Purchase, TrackedItem)
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
    , showCreateForm : Bool
    , addPurchase : Bool
    , newPurchase : Maybe NewPurchase
    , newPurchaseDescription : String
    , newPurchaseYear : String
    , newPurchaseMonth : String
    , newPurchaseDay : String
    , newPurchaseAmount : String
    , newPurchasePrice : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , newItemName = ""
      , showCreateForm = False
      , addPurchase = False
      , newPurchase = Nothing
      , newPurchaseDescription = ""
      , newPurchaseYear = ""
      , newPurchaseMonth = ""
      , newPurchaseDay = ""
      , newPurchaseAmount = ""
      , newPurchasePrice = ""
      }
    , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
    )



-- UPDATE


type Msg
    = NewItemNameUpdated String
    | AddPurchaseClicked
    | NewTrackedItemSubmitted
    | ClearNewTransactionForm
    | ShowCreateForm
    | NewPurchaseInput PurchaseInputType String
    | TrackedItemCreated (Result Http.Error String)
    | TrackedItemApiResponded (Result Http.Error (List TrackedItem))


type PurchaseInputType
    = PurchaseDescription
    | PurchaseYear
    | PurchaseMonth
    | PurchaseDay
    | PurchaseAmount
    | PurchasePrice


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NewItemNameUpdated s ->
            ( { model | newItemName = s }
            , Effect.none
            )

        AddPurchaseClicked ->
            ( { model
                | addPurchase = True
                , newPurchase = Just (NewPurchase "" "" "" "" "" 0 0 0)
              }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( { model | showCreateForm = False }
            , Api.TrackedItem.create
                { onResponse = TrackedItemCreated
                , name = model.newItemName
                }
            )

        ClearNewTransactionForm ->
            ( { model
                | newItemName = ""
                , showCreateForm = False
                , addPurchase = False
              }
            , Effect.none
            )

        ShowCreateForm ->
            ( { model
                | showCreateForm =
                    if model.showCreateForm == True then
                        False

                    else
                        True
              }
            , Effect.none
            )

        NewPurchaseInput PurchaseDescription value ->
            ( { model | newPurchaseDescription = value }, Effect.none )

        NewPurchaseInput PurchaseYear value ->
            ( { model | newPurchaseYear = value }, Effect.none )

        NewPurchaseInput PurchaseMonth value ->
            ( { model | newPurchaseMonth = value }, Effect.none )

        NewPurchaseInput PurchaseDay value ->
            ( { model | newPurchaseDay = value }, Effect.none )

        NewPurchaseInput PurchaseAmount value ->
            ( { model | newPurchaseAmount = value }, Effect.none )

        NewPurchaseInput PurchasePrice value ->
            ( { model | newPurchasePrice = value }, Effect.none )

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
            ( model, Effect.none )


newPurchaseToPurchase : NewPurchase -> Purchase
newPurchaseToPurchase np =
    let
        purchaseDate : String -> String -> String -> String
        purchaseDate year month day =
            year ++ "-" ++ month ++ "-" ++ day
    in
    Purchase
        np.product_description
        (purchaseDate np.purchased_year np.purchased_month np.purchased_day)
        np.purchased_amount
        np.price
        np.interval_to_previous



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
                                viewTrackedItems trackedItems

                            Api.Failure _ ->
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


viewTrackedItems : List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems ts =
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
    Html.button [ class "button is-primary is-medium is-fullwidth", onClick ShowCreateForm ] [ Html.text "New Tracked Item" ]


viewCreateTrackedItemForm : Model -> Html Msg
viewCreateTrackedItemForm model =
    if model.showCreateForm == False then
        Html.div [] []

    else
        Html.div [ class "box pl-6 pr-6" ]
            [ Html.div [ class "is-full" ]
                [ Html.div [ class "field" ]
                    [ Html.label [ class "label" ] [ Html.text "Name" ]
                    , Html.div [ class "control" ] [ Html.input [ class "input", placeholder "Shampoo", type_ "text", onInput NewItemNameUpdated ] [] ]
                    ]
                , Html.label [ class "label" ] [ Html.text "Purchases" ]
                , if model.addPurchase then
                    Html.div [] [ showAddPurchase ]

                  else
                    Html.div [] []
                , Html.div [ class "field mt-2" ]
                    [ Html.button [ class "button is-dark", onClick AddPurchaseClicked ] [ Html.text "Add purchase" ] ]
                , Html.div [ class "mt-2 is-flex is-justify-content-space-around" ]
                    [ Html.button [ class "button is-primary", onClick NewTrackedItemSubmitted ] [ Html.text "Save" ]
                    , Html.button [ class "button is-secondary", onClick ClearNewTransactionForm ] [ Html.text "Cancel" ]
                    ]
                ]
            ]


showAddPurchase : Html Msg
showAddPurchase =
    Html.div [ class "field grid" ]
        [ Html.input [ class "input", placeholder "Product", onInput (NewPurchaseInput PurchaseDescription) ] []
        , Html.input [ class "input", placeholder "YYYY", onInput (NewPurchaseInput PurchaseYear) ] []
        , Html.input [ class "input", placeholder "MM", onInput (NewPurchaseInput PurchaseMonth) ] []
        , Html.input [ class "input", placeholder "DD", onInput (NewPurchaseInput PurchaseDay) ] []
        , Html.input [ class "input", placeholder "1", onInput (NewPurchaseInput PurchaseAmount) ] []
        , Html.input [ class "input", placeholder "1795", onInput (NewPurchaseInput PurchasePrice) ] []
        ]
