module Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, fill, padding, px, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
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
    Array.fromList
        [ { newStaticRoute
            | destination =
                Invalid "0.0.0.0/0" "Enter a classless destination network"
          }
        ]



-- UPDATE


type Msg
    = ChangeDestination Int String
    | ChangeRouter Int String
    | AddRoute
    | DeleteRoute Int
    | ResetRoutes


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

        DeleteRoute index ->
            if Array.length model == 1 then
                model

            else
                model
                    |> Array.toIndexedList
                    |> List.filterMap
                        (\( i, v ) ->
                            if i /= index then
                                Just v

                            else
                                Nothing
                        )
                    |> Array.fromList

        ResetRoutes ->
            init



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.column [ centerX, padding 20, Element.spacing 20 ]
        [ Element.indexedTable [ Element.spacing 20 ]
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

                    cols =
                        if Array.length model > 1 then
                            inputs
                                ++ [ { header = Element.none
                                     , width = Element.fillPortion 1
                                     , view =
                                        \i _ ->
                                            Input.button
                                                [ padding 12
                                                , Border.rounded 3
                                                , Background.color (rgb255 196 30 58)
                                                , Font.color (rgb255 255 255 255)
                                                ]
                                                { onPress = Just (DeleteRoute i)
                                                , label = text "Delete"
                                                }
                                     }
                                   ]

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
