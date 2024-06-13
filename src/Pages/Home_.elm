module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.TrackedItem
import Api.TrackedItemList
import Components.Input
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
    , newItemName : String
    , newDescription : String
    , newPurchasedDateYear : String
    , newPurchasedDateMonth : String
    , newPurchasedDateDay : String
    , newPurchasedAmount : Int
    , newPrice : Int
    , showCreateForm : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , newItemName = ""
      , newDescription = ""
      , newPurchasedDateYear = ""
      , newPurchasedDateMonth = ""
      , newPurchasedDateDay = ""
      , newPurchasedAmount = 0
      , newPrice = 0
      , showCreateForm = False
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
    | NewTrackedItemSubmitted
    | ClearNewTransactionForm
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
                    , viewCreateTrackedItemForm model.showCreateForm
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


viewCreateTrackedItemForm : Bool -> Html Msg
viewCreateTrackedItemForm showCreateForm =
    let
        divClass : String
        divClass =
            "mt-2 is-flex is-flex-direction-row is-justify-content-space-between"

        pClass : String
        pClass =
            "has-text-weight-medium"
    in
    if showCreateForm == False then
        Html.div [] []

    else
        Html.div [ class "box pl-6 pr-6" ]
            [ Html.span [ class "is-full" ]
                [ Html.h5 [ class "title is-5" ]
                    [ Html.input
                        [ class "input"
                        , placeholder "Name"
                        , type_ "text"
                        , onInput NewItemNameUpdated
                        ]
                        []
                    ]
                , Html.h6 [ class "mt-2 subtitle is-6 is-italic has-text-weight-medium" ]
                    [ Html.input
                        [ class "input"
                        , placeholder "Description"
                        , type_ "text"
                        , onInput NewDescriptionUpdated
                        ]
                        []
                    ]
                , Html.div [ class divClass ]
                    [ Html.p [ class pClass ] [ Html.text "Purchased year" ]
                    , Html.div [ class "is-flex is-flex-direction-row" ]
                        [ Html.p []
                            [ Html.input
                                [ class "input"
                                , placeholder "YYYY"
                                , type_ "text"
                                , onInput NewPurchasedDateYearUpdated
                                ]
                                []
                            ]
                        , Html.p []
                            [ Html.input
                                [ class "input"
                                , placeholder "MM"
                                , type_ "text"
                                , onInput NewPurchasedDateMonthUpdated
                                ]
                                []
                            ]
                        , Html.p []
                            [ Html.input
                                [ class "input"
                                , placeholder "DD"
                                , type_ "text"
                                , onInput NewPurchasedDateDayUpdated
                                ]
                                []
                            ]
                        ]
                    ]
                , Html.div [ class divClass ]
                    [ Html.p [ class pClass ] [ Html.text "Amount purchased" ]
                    , Html.p []
                        [ Html.input
                            [ class "input"
                            , placeholder "Amount"
                            , type_ "text"
                            , onInput NewPurchasedAmountUpdated
                            ]
                            []
                        ]
                    ]
                , Html.div [ class divClass ]
                    [ Html.p [ class pClass ] [ Html.text "Price paid" ]
                    , Html.p []
                        [ Html.input
                            [ class "input"
                            , placeholder "Price"
                            , type_ "text"
                            , onInput NewPriceUpdated
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "mt-2 is-flex is-justify-content-space-around" ]
                    [ Html.button [ class "button is-primary", onClick NewTrackedItemSubmitted ] [ Html.text "Save" ]
                    , Html.button [ class "button is-secondary", onClick ClearNewTransactionForm ] [ Html.text "Cancel" ]
                    ]
                ]
            ]
