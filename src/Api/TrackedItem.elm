module Api.TrackedItem exposing (create)

import Effect exposing (Effect)
import Http
import Json.Decode
import Json.Encode
import Shared.Model exposing (Purchase, TrackedItem)


create :
    { onResponse : Result Http.Error String -> msg
    , name : String
    , description : String
    }
    -> Effect msg
create options =
    let
        body : Json.Encode.Value
        body =
            Json.Encode.object
                [ ( "name", Json.Encode.string options.name )
                , ( "product_description", Json.Encode.string options.description )
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
