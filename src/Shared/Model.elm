module Shared.Model exposing (Model, NewPurchase, Purchase, TrackedItem)

{-| -}


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    {}


type alias TrackedItem =
    { name : String
    , purchases : List Purchase
    }


type alias Purchase =
    { product_description : String
    , purchased_date : String
    , purchased_amount : Int
    , price : Int
    , interval_to_previous : Int
    }


type alias NewPurchase =
    { tracked_item_name : String
    , product_description : String
    , purchased_year : String
    , purchased_month : String
    , purchased_day : String
    , purchased_amount : Int
    , price : Int
    , interval_to_previous : Int
    }
