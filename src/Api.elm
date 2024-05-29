module Api exposing (..)

import Http


type Data value
    = Loading
    | Success value
    | Failure Http.Error
