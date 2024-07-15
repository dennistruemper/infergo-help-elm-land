module Api.TrackedItem exposing (create)

import Effect exposing (Effect)
import Http
import Json.Encode
import Shared.Model exposing (Purchase, TrackedItem)


create :
    { onResponse : Result Http.Error String -> msg
    , name : String
    , purchase : Maybe Purchase
    }
    -> Effect msg
create options =
    let
        body : Json.Encode.Value
        body =
            case options.purchase of
                Just p ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string options.name )
                        , ( "initial_purchase"
                          , Json.Encode.object
                                [ ( "purchased_date", Json.Encode.string p.purchased_date )
                                , ( "product_description", Json.Encode.string p.product_description )
                                , ( "purchased_amount", Json.Encode.int p.purchased_amount )
                                , ( "price", Json.Encode.int p.price )
                                ]
                          )
                        ]

                Nothing ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string options.name ) ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = "http://localhost:8000/tracked-item"
                , body = Http.jsonBody body
                , expect = Http.expectString options.onResponse
                }
    in
    Effect.sendCmd cmd
