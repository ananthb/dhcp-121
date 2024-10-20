module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, main_, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import IP



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type InputField
    = Valid String
    | Invalid String String


type alias StaticRoute =
    { destination : InputField
    , router : InputField
    }


type alias Model =
    Array StaticRoute


newStaticRoute : StaticRoute
newStaticRoute =
    { destination = Invalid "" "Enter a classless destination network"
    , router = Invalid "" "Enter a valid IP address"
    }


init : Model
init =
    Array.fromList [ newStaticRoute ]



-- UPDATE


type Msg
    = ChangeDestination Int String
    | ChangeRouter Int String
    | AddRoute


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDestination index value ->
            case Array.get index model of
                Just route ->
                    model
                        |> Array.set index
                            (if value == "" then
                                { route
                                    | destination = Invalid value "Destination cannot be empty"
                                }

                             else
                                {- TODO: Validate destination -}
                                { route | destination = Valid value }
                            )

                Nothing ->
                    model

        ChangeRouter index value ->
            case Array.get index model of
                Just route ->
                    model
                        |> Array.set index
                            (if value == "" then
                                { route | router = Invalid value "Router cannot be empty" }

                             else if IP.validate value then
                                { route | router = Valid value }

                             else
                                { route | router = Invalid value "Enter a valid IP address" }
                            )

                Nothing ->
                    model

        AddRoute ->
            model |> Array.push newStaticRoute



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ model
            |> Array.foldl viewStaticRoutes []
            |> div []
        , button [ onClick AddRoute ] [ text "Add Route" ]
        ]


viewStaticRoutes : StaticRoute -> List (Html Msg) -> List (Html Msg)
viewStaticRoutes route routes =
    let
        index =
            List.length routes
    in
    routes
        ++ [ div []
                [ viewDestinationInput index route.destination
                , viewRouterInput index route.router
                ]
           ]


viewDestinationInput : Int -> InputField -> Html Msg
viewDestinationInput index inputField =
    let
        attrs =
            case inputField of
                Valid val ->
                    [ value val ]

                Invalid val errorMessage ->
                    [ value val, placeholder errorMessage ]
    in
    input (onInput (ChangeDestination index) :: attrs) []


viewRouterInput : Int -> InputField -> Html Msg
viewRouterInput index inputField =
    let
        attrs =
            case inputField of
                Valid val ->
                    [ value val ]

                Invalid val errorMessage ->
                    [ value val, placeholder errorMessage ]
    in
    input
        (onInput (ChangeRouter index)
            :: minlength 7
            :: maxlength 15
            :: size 15
            :: pattern "^(?>(\\d|[1-9]\\d{2}|1\\d\\d|2[0-4]\\d|25[0-5])\\.){3}(?1)$"
            :: attrs
        )
        []
