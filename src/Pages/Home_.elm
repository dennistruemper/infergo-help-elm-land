module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.TrackedItem exposing (init, new)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Purchase, TrackedItem)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { trackedItems : Api.Data (List TrackedItem)
    , trackedItemComponent : Components.TrackedItem.Model Bool
    , newItemName : String
    , showCreateForm : Bool
    , addPurchase : Bool
    , newPurchases : Dict Int NewPurchase
    , newPurchaseDescription : String
    , newPurchaseYear : String
    , newPurchaseMonth : String
    , newPurchaseDay : String
    , newPurchaseAmount : String
    , newPurchasePrice : String
    }


type alias NewPurchase =
    { product_description : String
    , year : String
    , month : String
    , day : String
    , amount : String
    , price : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , trackedItemComponent = Components.TrackedItem.init { name = "", purchases = [] }
      , newItemName = ""
      , showCreateForm = False
      , addPurchase = False
      , newPurchases = Dict.empty
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
    | AddPurchaseClicked Int
    | NewTrackedItemSubmitted
    | ClearNewTransactionForm
    | ShowCreateForm
    | NewPurchaseInput Int PurchaseInputType String
    | TrackedItemCreated (Result Http.Error String)
    | TrackedItemApiResponded (Result Http.Error (List TrackedItem))
    | TrackedItemExpanded (Components.TrackedItem.Msg TrackedItem Bool)


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

        AddPurchaseClicked index ->
            ( { model
                | addPurchase = True
                , newPurchases = Dict.insert index (NewPurchase "" "" "" "" "" "") model.newPurchases
              }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( { model
                | showCreateForm = False
                , newItemName = ""
                , addPurchase = False
                , newPurchases = Dict.empty
              }
            , Api.TrackedItem.create
                { onResponse = TrackedItemCreated
                , name = model.newItemName
                , purchases =
                    if Dict.size model.newPurchases == 0 then
                        Nothing

                    else
                        Just (List.map newPurchaseToSharedPurchase (Dict.values model.newPurchases))
                }
            )

        ClearNewTransactionForm ->
            ( { model
                | newItemName = ""
                , showCreateForm = False
                , addPurchase = False
                , newPurchases = Dict.empty
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
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | product_description = value })) model.newPurchases }
            , Effect.none
            )

        NewPurchaseInput index PurchaseYear value ->
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | year = value })) model.newPurchases }
            , Effect.none
            )

        NewPurchaseInput index PurchaseMonth value ->
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | month = value })) model.newPurchases }
            , Effect.none
            )

        NewPurchaseInput index PurchaseDay value ->
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | day = value })) model.newPurchases }
            , Effect.none
            )

        NewPurchaseInput index PurchaseAmount value ->
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | amount = value })) model.newPurchases }
            , Effect.none
            )

        NewPurchaseInput index PurchasePrice value ->
            ( { model | newPurchases = Dict.update index (Maybe.map (\p -> { p | price = value })) model.newPurchases }
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

        TrackedItemCreated (Ok _) ->
            ( model
            , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
            )

        TrackedItemCreated (Err _) ->
            ( model, Effect.none )

        TrackedItemExpanded innerMsg ->
            Components.TrackedItem.update
                { msg = innerMsg
                , model = model.trackedItemComponent
                , toModel = \trackedItem -> { model | trackedItem = trackedItem }
                , toMsg = TrackedItemExpanded
                }


newPurchaseToSharedPurchase : NewPurchase -> Purchase
newPurchaseToSharedPurchase np =
    Purchase
        np.product_description
        (toDate np.year np.month np.day)
        (String.toInt np.amount |> Maybe.withDefault 0)
        (String.toInt np.price |> Maybe.withDefault 0)
        0


toDate : String -> String -> String -> String
toDate year month day =
    year ++ "-" ++ month ++ "-" ++ day



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                Html.div []
                    (List.map
                        (\t ->
                            Components.TrackedItem.new
                                { purchases = t.purchases
                                , name = t.name
                                }
                                |> Components.TrackedItem.withIsExpanded False
                                |> Components.TrackedItem.view
                        )
                        ts
                    )
    in
    Html.div []
        [ Html.h1 [ class "title is-1 has-text-centered pt-5" ] [ Html.text "Tracked Items" ]
        , listView
        ]


viewShowCreateFormButton : Html Msg
viewShowCreateFormButton =
    Html.button [ class "button is-primary is-medium is-fullwidth mt-5", onClick ShowCreateForm ] [ Html.text "New Tracked Item" ]


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
                    Html.div [] (List.indexedMap (\i p -> showAddPurchase i p) (Dict.values model.newPurchases))

                  else
                    Html.div [] []
                , Html.div [ class "field mt-2" ]
                    [ Html.button [ class "button is-dark", onClick (AddPurchaseClicked (Dict.size model.newPurchases)) ] [ Html.text "Add purchase" ] ]
                , Html.div [ class "mt-2 is-flex is-justify-content-space-around" ]
                    [ Html.button [ class "button is-primary", onClick NewTrackedItemSubmitted ] [ Html.text "Save" ]
                    , Html.button [ class "button is-secondary", onClick ClearNewTransactionForm ] [ Html.text "Cancel" ]
                    ]
                ]
            ]


showAddPurchase : Int -> NewPurchase -> Html Msg
showAddPurchase index purchase =
    Html.div [ class "field grid" ]
        [ Html.input [ class "input", placeholder "Product", value purchase.product_description, onInput (NewPurchaseInput index PurchaseDescription) ] []
        , Html.input [ class "input", placeholder "YYYY", onInput (NewPurchaseInput index PurchaseYear) ] []
        , Html.input [ class "input", placeholder "MM", onInput (NewPurchaseInput index PurchaseMonth) ] []
        , Html.input [ class "input", placeholder "DD", onInput (NewPurchaseInput index PurchaseDay) ] []
        , Html.input [ class "input", placeholder "1", onInput (NewPurchaseInput index PurchaseAmount) ] []
        , Html.input [ class "input", placeholder "1795", onInput (NewPurchaseInput index PurchasePrice) ] []
        ]
