module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, main_, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import IP



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias StaticRoute =
    { destination : String
    , router : String
    }


type InputField
    = Valid String
    | Invalid String String


type alias Model =
    { routes : List StaticRoute
    , destination : InputField
    , router : InputField
    }


init : Model
init =
    { routes = []
    , destination = Invalid "" "Enter a classless destination network"
    , router = Invalid "" "Enter a valid IP address"
    }



-- UPDATE


type Msg
    = ChangeDestination String
    | ChangeRouter String
    | AddRoute


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDestination value ->
            if value == "" then
                { model | destination = Invalid value "Destination cannot be empty" }

            else
                {- TODO: Validate destination -}
                { model | destination = Valid value }

        ChangeRouter value ->
            if value == "" then
                { model | router = Invalid value "Router cannot be empty" }

            else if IP.validate value then
                { model | router = Valid value }

            else
                { model | router = Invalid value "Enter a valid IP address" }

        AddRoute ->
            case ( model.destination, model.router ) of
                ( Valid destination, Valid router ) ->
                    { model
                        | routes = { destination = destination, router = router } :: model.routes
                        , destination = Invalid "" "Enter a classless destination network"
                        , router = Invalid "" "Enter a valid IP address"
                    }

                _ ->
                    {- TODO: Show error message -}
                    model



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ Html.form [ onSubmit AddRoute ]
            [ viewDestinationInput model.destination
            , viewRouterInput model.router
            , button [ type_ "submit" ] [ text "Add Route" ]
            ]
        , div [] (List.map viewRoute model.routes)
        ]


viewDestinationInput : InputField -> Html Msg
viewDestinationInput inputField =
    let
        attrs =
            case inputField of
                Valid val ->
                    [ value val ]

                Invalid val errorMessage ->
                    [ value val, placeholder errorMessage ]
    in
    input (onInput ChangeDestination :: attrs) []


viewRouterInput : InputField -> Html Msg
viewRouterInput inputField =
    let
        attrs =
            case inputField of
                Valid val ->
                    [ value val ]

                Invalid val errorMessage ->
                    [ value val, placeholder errorMessage ]
    in
    input
        (onInput ChangeRouter
            :: minlength 7
            :: maxlength 15
            :: size 15
            :: pattern "^(?>(\\d|[1-9]\\d{2}|1\\d\\d|2[0-4]\\d|25[0-5])\\.){3}(?1)$"
            :: attrs
        )
        []


viewRoute : StaticRoute -> Html Msg
viewRoute route =
    div [] [ div [] [ text route.destination ], div [] [ text route.router ] ]
