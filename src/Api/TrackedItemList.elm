module Api.TrackedItemList exposing (getAll)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, null, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
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


decoder : Decoder (List TrackedItem)
decoder =
    list trackedItemDecoder


trackedItemDecoder : Decoder TrackedItem
trackedItemDecoder =
    succeed TrackedItem
        |> required "name" string
        |> optional "purchases" (list purchaseDecoder) []


purchaseDecoder : Decoder Purchase
purchaseDecoder =
    succeed Purchase
        |> required "product_description" string
        |> required "purchased_date" string
        |> required "purchased_amount" int
        |> required "price" int
        |> required "interval_to_previous" int
