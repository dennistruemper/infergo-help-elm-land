module Pages.Home_ exposing (Model, Msg, page)

import Components.Input.Text
import Components.TrackedItem
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
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
    { trackedItems : List TrackedItem
    , newTrackedItemName : String
    , newTrackedItemDescription : String
    , newTrackedItemPurchase : Maybe Purchase
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { trackedItems =
            [ TrackedItem "First" "Description" []
            , TrackedItem "Second" "Description" []
            ]
      , newTrackedItemName = ""
      , newTrackedItemDescription = ""
      , newTrackedItemPurchase = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NewTrackedItemNameUpdated String
    | NewTrackedItemDescriptionUpdated String
    | NewTrackedItemSubmitted


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
            [ Html.div [ class "flex place-content-around" ] [ viewTrackedItems model.trackedItems ]
            , viewCreateTrackedItemForm
            ]
        ]
    }


viewTrackedItems : List Shared.Model.TrackedItem -> Html Msg
viewTrackedItems ts =
    Html.div
        [ class "border flex has-text-centered " ]
        [ Html.ul [ class "list text-center" ] (List.map Components.TrackedItem.view ts) ]


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
