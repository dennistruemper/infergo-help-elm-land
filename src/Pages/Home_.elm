module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Array exposing (Array)
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
    , showCreateForm : Bool
    , addPurchase : Bool
    , newPurchases : Array Purchase
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , purchaseInput = Components.PurchaseInput.init
      , newItemName = ""
      , showCreateForm = False
      , addPurchase = False
      , newPurchases = Array.empty
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
    | NewPurchaseInput Int PurchaseInputType String
    | PurchaseInputCompleted (Components.PurchaseInput.Msg Msg)
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
                , newPurchases = Array.append model.newPurchases (Array.fromList [ Purchase "" "" 0 0 0 ])
              }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( { model | showCreateForm = False }
            , Api.TrackedItem.create
                { onResponse = TrackedItemCreated
                , name = model.newItemName
                , purchases = Just model.newPurchases

                --case model.newPurchases of
                --    Just purchases ->
                --        purchases
                --    Nothing ->
                --        []
                }
            )

        ClearNewTransactionForm ->
            ( { model
                | newItemName = ""
                , showCreateForm = False
                , addPurchase = False
                , newPurchases = Array.empty
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

        NewPurchaseInput index PurchaseDescription value ->
            let
                purchase : Purchase
                purchase =
                    case Array.get index model.newPurchases of
                        Just p ->
                            p

                        Nothing ->
                            Purchase "" "" 0 0 0
            in
            ( model, Effect.none )

        NewPurchaseInput index PurchaseYear value ->
            ( model, Effect.none )

        NewPurchaseInput index PurchaseMonth value ->
            ( model, Effect.none )

        NewPurchaseInput index PurchaseDay value ->
            ( model, Effect.none )

        NewPurchaseInput index PurchaseAmount value ->
            ( model, Effect.none )

        NewPurchaseInput index PurchasePrice value ->
            ( model, Effect.none )

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


viewTrackedItems : Bool -> List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems _ ts =
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
    Html.button [ class "button is-primary is-medium is-fullwidth", onClick ShowCreateForm ] [ Html.text "New Transaction" ]


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
                , Html.div [] (List.indexedMap (\i p -> showAddPurchase i p) (Array.toList model.newPurchases))
                , Html.div [ class "field mt-2" ]
                    [ Html.button [ class "button is-dark", onClick AddPurchaseClicked ] [ Html.text "Add purchase" ] ]
                , Html.div [ class "mt-2 is-flex is-justify-content-space-around" ]
                    [ Html.button [ class "button is-primary", onClick NewTrackedItemSubmitted ] [ Html.text "Save" ]
                    , Html.button [ class "button is-secondary", onClick ClearNewTransactionForm ] [ Html.text "Cancel" ]
                    ]
                ]
            ]


showAddPurchase : Int -> Purchase -> Html Msg
showAddPurchase index purchase =
    Html.div [ class "field grid" ]
        [ Html.input [ class "input", placeholder "Product", onInput (NewPurchaseInput index PurchaseDescription) ] []
        , Html.input [ class "input", placeholder "YYYY" ] []
        , Html.input [ class "input", placeholder "MM" ] []
        , Html.input [ class "input", placeholder "DD" ] []
        , Html.input [ class "input", placeholder "1" ] []
        , Html.input [ class "input", placeholder "1795" ] []
        ]



--[ Components.PurchaseInput.new
--    { model = model
--    , toMsg = PurchaseInputCompleted
--    }
--    |> Components.PurchaseInput.view
--]
