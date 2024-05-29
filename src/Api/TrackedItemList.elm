module Api.TrackedItemList exposing (getAll)

import Effect exposing (Effect)
import Http
import Json.Decode
import Shared.Model exposing (Purchase, TrackedItem)


getAll : { onResponse : Result Http.Error (List TrackedItem) -> msg } -> Effect msg
getAll options =
    let
        cmd : Cmd msg
        cmd =
            Http.get
                { url = "http://localhost:8000/tracked-items"
                , expect = Http.expectJson options.onResponse decoder
                }
    in
    Effect.sendCmd cmd


decoder : Json.Decode.Decoder (List TrackedItem)
decoder =
    Json.Decode.list trackedItemDecoder


trackedItemDecoder : Json.Decode.Decoder TrackedItem
trackedItemDecoder =
    Json.Decode.map3 TrackedItem
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "product_description" Json.Decode.string)
        (Json.Decode.list purchaseDecoder)


purchaseDecoder : Json.Decode.Decoder Purchase
purchaseDecoder =
    Json.Decode.map4 Purchase
        (Json.Decode.field "purchased_date" Json.Decode.string)
        (Json.Decode.field "purchased_amount" Json.Decode.int)
        (Json.Decode.field "price" Json.Decode.int)
        (Json.Decode.field "interval_to_previous" Json.Decode.int)
