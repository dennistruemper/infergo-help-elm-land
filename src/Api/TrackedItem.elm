module Api.TrackedItem exposing (ApiPurchase, ApiTrackedItem, create)

import Effect exposing (Effect)
import Http
import Json.Encode
import Shared.Model


type alias ApiTrackedItem =
    { name : String
    , purchases : List ApiPurchase
    }


type alias ApiPurchase =
    { product_description : String
    , purchased_date : String
    , purchased_amount : Int
    , price : Int
    , interval_to_previous : Int
    }


create :
    { onResponse : Result Http.Error String -> msg
    , name : String
    , purchases : Maybe (List ApiPurchase)
    }
    -> Effect msg
create options =
    let
        body : Json.Encode.Value
        body =
            case options.purchases of
                Just ps ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string options.name )
                        , ( "purchases", Json.Encode.list encodePurchase ps )
                        ]

                Nothing ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string options.name ) ]

        encodePurchase : ApiPurchase -> Json.Encode.Value
        encodePurchase p =
            Json.Encode.object
                [ ( "purchased_date", Json.Encode.string p.purchased_date )
                , ( "product_description", Json.Encode.string p.product_description )
                , ( "purchased_amount", Json.Encode.int p.purchased_amount )
                , ( "price", Json.Encode.int p.price )
                ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = "http://localhost:8000/tracked-item"
                , body = Http.jsonBody body
                , expect = Http.expectString options.onResponse
                }
    in
    Effect.sendCmd cmd
