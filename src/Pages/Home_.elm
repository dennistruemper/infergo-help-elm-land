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
    , newTrackedItemName : String
    , newTrackedItemDescription : String
    , newTrackedItemPurchase : Maybe Purchase
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems = Api.Loading
      , newTrackedItemName = ""
      , newTrackedItemDescription = ""
      , newTrackedItemPurchase = Nothing
      }
    , Api.TrackedItemList.getAll { onResponse = TrackedItemApiResponded }
    )



-- UPDATE


type Msg
    = NewTrackedItemNameUpdated String
    | NewTrackedItemDescriptionUpdated String
    | NewTrackedItemSubmitted
    | TrackedItemCreated (Result Http.Error String)
    | TrackedItemApiResponded (Result Http.Error (List TrackedItem))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NewTrackedItemNameUpdated s ->
            ( { model | newTrackedItemName = s }
            , Effect.none
            )

        NewTrackedItemDescriptionUpdated s ->
            ( { model | newTrackedItemDescription = s }
            , Effect.none
            )

        NewTrackedItemSubmitted ->
            ( model
            , Api.TrackedItem.create { onResponse = TrackedItemCreated, name = model.newTrackedItemName, description = model.newTrackedItemDescription }
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
            [ class "p-5"
            , style "width" "inherit"
            ]
            [ Html.div [ class "grid grid-cols-2" ]
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
        [ class "flex flex-col" ]
        [ Html.h1 [ class "" ] [ Html.text "Overview" ]
        , Html.div [ class "" ] (List.map Components.TrackedItem.view ts)
        ]


viewCreateTrackedItemForm : Html Msg
viewCreateTrackedItemForm =
    Html.div
        [ class "flex place-content-around" ]
        [ Html.div []
            [ Html.h1 [ class "h1" ] [ Html.text "Create new tracked item" ]
            , Components.Input.Text.view
                { label = "Tracked Item Name"
                , placeholder = "Tracked Item Name"
                , onInput = NewTrackedItemNameUpdated
                }
            , Components.Input.Text.view
                { label = "Tracked Item Description"
                , placeholder = "Tracked Item Description"
                , onInput = NewTrackedItemDescriptionUpdated
                }
            , Html.button [ class "border button", onClick NewTrackedItemSubmitted ] [ Html.text "Submit" ]
            ]
        ]
