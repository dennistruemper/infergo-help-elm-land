module Components.PurchaseInput exposing (Model, Msg, PurchaseInput, init, new, update, view)

import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick)
import Shared.Model exposing (Purchase)



-- SETTINGS


type PurchaseInput msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , onSave : Maybe msg
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    }
    -> PurchaseInput msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onSave = Nothing
        }



-- MODEL


type Model
    = Model
        { productInput : String
        , yearInput : String
        , monthInput : String
        , dayInput : String
        , amountInput : String
        , priceInput : String
        }


init : Model
init =
    Model
        { productInput = ""
        , yearInput = ""
        , monthInput = ""
        , dayInput = ""
        , amountInput = ""
        , priceInput = ""
        }



-- UPDATE


type Msg msg
    = ProductInputUpdated String
    | YearInputUpdated String
    | MonthInputUpdated String
    | DayInputUpdated String
    | AmountInputUpdated String
    | PriceInputUpdated String
    | InputCompleted { onSave : Maybe msg }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            ProductInputUpdated pi ->
                ( Model { model | productInput = pi }
                , Effect.none
                )

            YearInputUpdated y ->
                ( Model { model | yearInput = y }
                , Effect.none
                )

            MonthInputUpdated m ->
                ( Model { model | monthInput = m }
                , Effect.none
                )

            DayInputUpdated d ->
                ( Model { model | dayInput = d }
                , Effect.none
                )

            AmountInputUpdated a ->
                ( Model { model | amountInput = a }
                , Effect.none
                )

            PriceInputUpdated p ->
                ( Model { model | priceInput = p }
                , Effect.none
                )

            InputCompleted data ->
                ( Model model
                , case data.onSave of
                    Just onSave ->
                        Effect.sendMsg onSave

                    Nothing ->
                        Effect.none
                )



-- VIEW


view : PurchaseInput msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        onPurchaseSave : msg
        onPurchaseSave =
            settings.toMsg <|
                case settings.onSave of
                    Just onSave ->
                        InputCompleted { onSave = Just onSave }

                    Nothing ->
                        InputCompleted { onSave = Nothing }
    in
    Html.div [ class "grid" ]
        [ Html.input [ class "input", placeholder "Product" ] []
        , Html.input [ class "input", placeholder "YYYY" ] []
        , Html.input [ class "input", placeholder "MM" ] []
        , Html.input [ class "input", placeholder "DD" ] []
        , Html.input [ class "input", placeholder "1" ] []
        , Html.input [ class "input", placeholder "1795" ] []
        , Html.button [ class "button is-primary", onClick onPurchaseSave ] []
        ]
