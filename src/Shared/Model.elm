module Shared.Model exposing (Model, Purchase, TrackedItem)

{-| -}


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { trackedItems : List TrackedItem }


type alias TrackedItem =
    { name : String
    , purchases : List Purchase
    , isExpanded : Bool
    }


type alias Purchase =
    { product_description : String
    , purchased_date : String
    , purchased_amount : Int
    , price : Int
    , interval_to_previous : Int
    }
