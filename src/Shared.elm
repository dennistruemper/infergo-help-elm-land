module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (Purchase, TrackedItem)
import Shared.Msg



-- FLAGS


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}



-- INIT


type alias Model =
    Shared.Model.Model


trackedItemExampleShampoo : TrackedItem
trackedItemExampleShampoo =
    { name = "Shampoo"
    , id = "1"
    , purchases = [ purchaseExampleShampoo1, purchaseExampleShampoo2 ]
    , isExpanded = False
    }


purchaseExampleShampoo1 : Purchase
purchaseExampleShampoo1 =
    { product_description = "Shampoo with strawberry taste"
    , purchased_date = "2021-01-01"
    , purchased_amount = 1
    , price = 10
    , interval_to_previous = 0
    }


purchaseExampleShampoo2 : Purchase
purchaseExampleShampoo2 =
    { product_description = "Shampoo"
    , purchased_date = "2021-01-02"
    , purchased_amount = 1
    , price = 10
    , interval_to_previous = 1
    }


trackedItemExampleToothpaste : TrackedItem
trackedItemExampleToothpaste =
    { name = "Toothpaste"
    , id = "2"
    , purchases = [ purchaseExampleToothpaste1, purchaseExampleToothpaste2 ]
    , isExpanded = False
    }


purchaseExampleToothpaste1 : Purchase
purchaseExampleToothpaste1 =
    { product_description = "Toothpaste with strawberry taste"
    , purchased_date = "2021-01-01"
    , purchased_amount = 1
    , price = 10
    , interval_to_previous = 0
    }


purchaseExampleToothpaste2 : Purchase
purchaseExampleToothpaste2 =
    { product_description = "Toothpaste"
    , purchased_date = "2021-01-02"
    , purchased_amount = 1
    , price = 10
    , interval_to_previous = 1
    }


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    ( { trackedItems =
            [ trackedItemExampleShampoo
            , trackedItemExampleToothpaste
            ]
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.Nothing ->
            ( model
            , Effect.none
            )

        Shared.Msg.TrackedItemCreated ti ->
            ( model
            , Effect.none
            )

        Shared.Msg.ToggleTrackedItem trackedItemId ->
            ( model |> toggleTrackedItem trackedItemId
            , Effect.none
            )


toggleTrackedItem : String -> Model -> Model
toggleTrackedItem id model =
    let
        toggleTrackedItemHelper : TrackedItem -> TrackedItem
        toggleTrackedItemHelper ti =
            if ti.id == id then
                { ti | isExpanded = not ti.isExpanded }

            else
                ti
    in
    { model | trackedItems = List.map toggleTrackedItemHelper model.trackedItems }



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
