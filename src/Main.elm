module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, fill, padding, px, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import IP
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



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


newModel : Model
newModel =
    Array.fromList [ { newStaticRoute | destination = Valid "0.0.0.0/0" } ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( newModel, Cmd.none )


calculateOption121 : Model -> Maybe String
calculateOption121 model =
    let
        calc : StaticRoute -> Maybe String -> Maybe String
        calc route acc =
            case ( route.destination, route.router ) of
                ( Valid dest, Valid router ) ->
                    {- TODO: actually calculate the option 121 -}
                    Just (router ++ "121 " ++ dest ++ " " ++ router ++ " ")

                _ ->
                    acc
    in
    Array.foldl calc Nothing model



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ChangeDestination Int String
    | ChangeRouter Int String
    | AddRoute
    | DeleteRoute Int
    | ResetRoutes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        ChangeDestination index value ->
            case Array.get index model of
                Just route ->
                    let
                        changeDestination =
                            Array.set index
                                (if value == "" then
                                    { route
                                        | destination = Invalid value "Destination cannot be empty"
                                    }

                                 else
                                    {- TODO: Validate destination -}
                                    { route | destination = Valid value }
                                )
                    in
                    ( changeDestination model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangeRouter index value ->
            case Array.get index model of
                Just route ->
                    let
                        changeRouter =
                            Array.set index
                                (if value == "" then
                                    { route | router = Invalid value "Router cannot be empty" }

                                 else if IP.validate value then
                                    { route | router = Valid value }

                                 else
                                    { route | router = Invalid value "Enter a valid IP address" }
                                )
                    in
                    ( changeRouter model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        AddRoute ->
            ( model |> Array.push newStaticRoute, Cmd.none )

        DeleteRoute index ->
            if Array.length model == 1 then
                ( model, Cmd.none )

            else
                let
                    deleteRoute =
                        Array.toIndexedList
                            >> List.filterMap
                                (\( i, v ) ->
                                    if i /= index then
                                        Just v

                                    else
                                        Nothing
                                )
                            >> Array.fromList
                in
                ( deleteRoute model, Cmd.none )

        ResetRoutes ->
            ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "DHCPv4 Option 121 Calculator"
    , body =
        [ Element.column [ centerX, padding 20, Element.spacing 20 ]
            [ viewOption121 model
            , Element.indexedTable [ Element.spacing 20 ]
                { data = Array.toList model
                , columns =
                    let
                        inputs =
                            [ { header = text "Destination"
                              , width = Element.fillPortion 3
                              , view = viewDestination
                              }
                            , { header = text "Router"
                              , width = Element.fillPortion 3
                              , view = viewRouter
                              }
                            ]

                        delete =
                            { header = Element.none
                            , width = Element.fillPortion 1
                            , view = viewDeleteButton
                            }

                        cols =
                            if Array.length model > 1 then
                                inputs ++ [ delete ]

                            else
                                inputs
                    in
                    cols
                }
            , Element.row
                [ Element.spacing 5, Element.alignRight ]
                [ Input.button
                    [ padding 12
                    , Border.rounded 3
                    , Border.solid
                    , Background.color (rgb255 8 143 143)
                    , Font.color (rgb255 255 255 255)
                    ]
                    { onPress = Just AddRoute
                    , label = text "Add Route"
                    }
                , Input.button
                    [ padding 12
                    , Border.rounded 3
                    , Border.solid
                    , Background.color (rgb255 129 65 65)
                    , Font.color (rgb255 255 255 255)
                    ]
                    { onPress = Just ResetRoutes
                    , label = text "Reset"
                    }
                ]
            ]
            |> Element.layout []
        ]
    }


viewOption121 : Model -> Element Msg
viewOption121 model =
    let
        option121 =
            case calculateOption121 model of
                Just val ->
                    text val

                Nothing ->
                    text "Some routes are invalid"
    in
    [ option121 ]
        |> Element.column [ centerX, padding 20, Element.spacing 20 ]


viewDestination : Int -> StaticRoute -> Element Msg
viewDestination index route =
    let
        attrs =
            case route.destination of
                Valid val ->
                    { text = val, placeholder = Nothing }

                Invalid val errorMessage ->
                    { text = val
                    , placeholder =
                        text errorMessage
                            |> Input.placeholder []
                            |> Just
                    }
    in
    { onChange = ChangeDestination index
    , text = attrs.text
    , label = Input.labelHidden "Destination"
    , placeholder = attrs.placeholder
    }
        |> Input.text [ width (px 200) ]


viewRouter : Int -> StaticRoute -> Element Msg
viewRouter index route =
    let
        attrs =
            case route.router of
                Valid val ->
                    { text = val, placeholder = Nothing }

                Invalid val errorMessage ->
                    { text = val
                    , placeholder =
                        text errorMessage
                            |> Input.placeholder []
                            |> Just
                    }
    in
    { onChange = ChangeRouter index
    , text = attrs.text
    , label = Input.labelHidden "Router"
    , placeholder = attrs.placeholder
    }
        |> Input.text [ width fill ]


viewDeleteButton : Int -> StaticRoute -> Element Msg
viewDeleteButton index _ =
    Input.button
        [ padding 12
        , Border.rounded 3
        , Background.color (rgb255 196 30 58)
        , Font.color (rgb255 255 255 255)
        ]
        { onPress = Just (DeleteRoute index)
        , label = text "Delete"
        }
