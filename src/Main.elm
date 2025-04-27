module Main exposing (main)

import Array exposing (Array)
import Base
import Browser
import Browser.Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Element exposing (Element, centerX, padding, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Task
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
    = Valid String String
    | Invalid String String


type alias StaticRoute =
    { destination : InputField
    , router : InputField
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , device : Element.Device
    , routes : Array StaticRoute
    , option121 : Maybe String
    }


newStaticRoute : StaticRoute
newStaticRoute =
    { destination = Invalid "" "Enter a destination subnet"
    , router = Invalid "" "Enter a router IP address"
    }


newStaticRoutes : Array StaticRoute
newStaticRoutes =
    Array.fromList [ { newStaticRoute | destination = Valid "0.0.0.0/0" "00" } ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    -- TODO: Parse the URL and set the routes
    ( { key = key
      , url = url
      , device = { class = Element.Phone, orientation = Element.Portrait }
      , routes = newStaticRoutes
      , option121 = Nothing
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotViewport Browser.Dom.Viewport
    | SetScreenSize Int Int
    | ChangeDestination Int String
    | ChangeRouter Int String
    | AddRoute
    | DeleteRoute Int
    | ResetRoutes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        GotViewport viewport ->
            ( { model
                | device =
                    Element.classifyDevice
                        { width = floor viewport.viewport.width
                        , height = floor viewport.viewport.height
                        }
              }
            , Cmd.none
            )

        SetScreenSize width height ->
            ( { model
                | device =
                    Element.classifyDevice
                        { width = width
                        , height = height
                        }
              }
            , Cmd.none
            )

        ChangeDestination index value ->
            case Array.get index model.routes of
                Just route ->
                    let
                        changeDestination =
                            Array.set index
                                (if value == "" then
                                    { route
                                        | destination = Invalid value "Destination cannot be empty"
                                    }

                                 else
                                    case parseDestination value of
                                        Just hex ->
                                            { route | destination = Valid value hex }

                                        Nothing ->
                                            { route
                                                | destination =
                                                    Invalid value "Enter a valid destination network"
                                            }
                                )

                        routes =
                            changeDestination model.routes

                        ( opt121, cmd ) =
                            updateOption121 model.key routes
                    in
                    ( { model
                        | routes = routes
                        , option121 = opt121
                      }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangeRouter index value ->
            case Array.get index model.routes of
                Just route ->
                    let
                        changeRouter =
                            Array.set index
                                (if value == "" then
                                    { route | router = Invalid value "Router cannot be empty" }

                                 else
                                    case parseRouter value of
                                        Just hex ->
                                            { route | router = Valid value hex }

                                        Nothing ->
                                            { route | router = Invalid value "Enter a valid IP address" }
                                )

                        routes =
                            changeRouter model.routes

                        ( opt121, cmd ) =
                            updateOption121 model.key routes
                    in
                    ( { model
                        | routes = routes
                        , option121 = opt121
                      }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddRoute ->
            let
                routes =
                    Array.push newStaticRoute model.routes

                ( opt121, cmd ) =
                    updateOption121 model.key routes
            in
            ( { model
                | routes = routes
                , option121 = opt121
              }
            , cmd
            )

        DeleteRoute index ->
            if Array.length model.routes == 1 then
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

                    routes =
                        deleteRoute model.routes

                    ( opt121, cmd ) =
                        updateOption121 model.key routes
                in
                ( { model
                    | routes = routes
                    , option121 = opt121
                  }
                , cmd
                )

        ResetRoutes ->
            ( { model
                | routes = newStaticRoutes
                , option121 = Nothing
              }
            , Nav.pushUrl model.key "#"
            )


updateOption121 : Nav.Key -> Array StaticRoute -> ( Maybe String, Cmd Msg )
updateOption121 key routes =
    let
        calc : StaticRoute -> Maybe String -> Maybe String
        calc route option =
            case ( option, route.destination, route.router ) of
                ( Just opt, Valid _ hexDest, Valid _ hexRouter ) ->
                    Just (opt ++ hexDest ++ hexRouter)

                _ ->
                    Nothing
    in
    case Array.foldl calc (Just "") routes of
        Just opt ->
            ( Just opt, Nav.pushUrl key opt )

        Nothing ->
            ( Nothing, Cmd.none )


{-| Parse the destination network in CIDR notation into a hex string.
The CIDR notation is converted to a hex string and prepended to the
destination network hex string.
-}
parseDestination : String -> Maybe String
parseDestination network =
    case String.split "/" network of
        [ netStr, cidrStr ] ->
            cidrStr
                |> String.toInt
                |> Maybe.andThen
                    (\cidr ->
                        if cidr > 0 && cidr < 32 then
                            let
                                pre =
                                    if cidr < 16 then
                                        "0"

                                    else
                                        ""

                                cidrHex =
                                    pre ++ Base.fromInt Base.b16 cidr
                            in
                            netStr
                                |> parseRouter
                                |> Maybe.andThen (\hex -> Just (cidrHex ++ hex))

                        else
                            Nothing
                    )

        [ netStr ] ->
            netStr
                |> parseRouter
                -- /32 is the default subnet mask which
                |> Maybe.andThen (\hex -> Just ("20" ++ hex))

        _ ->
            Nothing


{-| Parse the router IP address into a hex string.
The router IP address is converted to a hex string and prepended to the
destination network hex string.
-}
parseRouter : String -> Maybe String
parseRouter network =
    case String.split "." network of
        [ a, b, c, d ] ->
            List.foldl
                (\octet acc ->
                    octet
                        |> String.toInt
                        |> Maybe.andThen
                            (\val ->
                                if val >= 0 && val <= 255 then
                                    let
                                        pre =
                                            if val < 16 then
                                                "0"

                                            else
                                                ""

                                        x =
                                            pre ++ Base.fromInt Base.b16 val
                                    in
                                    Maybe.andThen (\ac -> Just (ac ++ x)) acc

                                else
                                    Nothing
                            )
                )
                (Just "")
                [ a, b, c, d ]

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onResize SetScreenSize ]



-- VIEW


black : Element.Color
black =
    Element.rgb255 0 0 0


cardinal : Element.Color
cardinal =
    Element.rgb255 196 30 58


white : Element.Color
white =
    Element.rgb255 255 255 255


bluegreen : Element.Color
bluegreen =
    Element.rgb255 8 143 143


crimson : Element.Color
crimson =
    Element.rgb255 129 65 65


darkgreen : Element.Color
darkgreen =
    Element.rgb255 2 48 32


grey : Element.Color
grey =
    Element.rgb255 128 128 128


view : Model -> Browser.Document Msg
view model =
    { title = "DHCPv4 Option 121 Calculator"
    , body =
        [ Element.column
            [ centerX
            , padding 20
            , Element.spacing 20
            , Region.mainContent
            ]
            [ viewHeader model.device
            , viewOption121 model
            , viewRoutesTable model
            , viewButtons
            , viewFooter
            ]
            |> Element.layout []
        ]
    }


viewHeader : Element.Device -> Element Msg
viewHeader device =
    let
        attrs =
            [ Element.spacing 5
            , Element.padding 20
            , Element.centerX
            ]

        children =
            [ Element.image [ Element.centerX ]
                { src = "favicon-96x96.png"
                , description = "Circuit traces & gear logo"
                }
            , text "DHCPv4 Option 121 Calculator"
                |> Element.el
                    [ Region.heading 1
                    , Font.size 24
                    , Font.bold
                    , Font.color black
                    ]
            ]
    in
    case device.class of
        Element.Phone ->
            Element.column attrs children

        _ ->
            Element.row attrs children


viewOption121 : Model -> Element Msg
viewOption121 model =
    let
        attrs =
            [ Element.spacing 20
            , padding 20
            , centerX
            , Border.solid
            , Border.rounded 3
            , Border.color grey
            , Border.width 1
            ]

        opt =
            model.option121
                |> Maybe.map (\o -> "Option 121: " ++ o)
                |> Maybe.withDefault "Some routes are invalid"
    in
    Element.column attrs [ text opt ]


viewRoutesTable : Model -> Element Msg
viewRoutesTable model =
    Element.indexedTable
        [ Element.spacing 20 ]
        { data = Array.toList model.routes
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
                    if Array.length model.routes > 1 then
                        inputs ++ [ delete ]

                    else
                        inputs
            in
            cols
        }


viewDestination : Int -> StaticRoute -> Element Msg
viewDestination index route =
    let
        attrs =
            case route.destination of
                Valid val _ ->
                    { text = val, label = Input.labelHidden "Destination" }

                Invalid val errorMessage ->
                    { text = val
                    , label = Input.labelBelow [] (text errorMessage)
                    }
    in
    { onChange = ChangeDestination index
    , text = attrs.text
    , label = attrs.label
    , placeholder = text "Destination subnet" |> Input.placeholder [] |> Just
    }
        |> Input.text [ width <| Element.px 200 ]


viewRouter : Int -> StaticRoute -> Element Msg
viewRouter index route =
    let
        attrs =
            case route.router of
                Valid val _ ->
                    { text = val, label = Input.labelHidden "Router" }

                Invalid val errorMessage ->
                    { text = val
                    , label = Input.labelBelow [] (text errorMessage)
                    }
    in
    { onChange = ChangeRouter index
    , text = attrs.text
    , label = attrs.label
    , placeholder = text "Router IP address" |> Input.placeholder [] |> Just
    }
        |> Input.text [ width Element.fill ]


viewDeleteButton : Int -> StaticRoute -> Element Msg
viewDeleteButton index _ =
    Input.button
        [ padding 12
        , Border.rounded 3
        , Background.color cardinal
        , Font.color white
        ]
        { onPress = Just (DeleteRoute index)
        , label = text "Delete"
        }


viewButtons : Element Msg
viewButtons =
    Element.row
        [ Element.spacing 5, Element.alignRight ]
        [ Input.button
            [ padding 12
            , Border.rounded 3
            , Border.solid
            , Background.color bluegreen
            , Font.color white
            ]
            { onPress = Just AddRoute
            , label = text "Add Route"
            }
        , Input.button
            [ padding 12
            , Border.rounded 3
            , Border.solid
            , Background.color crimson
            , Font.color white
            ]
            { onPress = Just ResetRoutes
            , label = text "Reset"
            }
        ]


viewFooter : Element Msg
viewFooter =
    let
        linkAttrs =
            [ Font.color darkgreen
            , Font.underline
            ]
    in
    Element.column
        [ Region.footer
        , Element.spacing 5
        , Element.padding 20
        , Font.size 14
        , Font.color grey
        , Font.light
        ]
        [ Element.row []
            [ text "Made with ❤️ by "
            , Element.newTabLink
                linkAttrs
                { url = "https://devhuman.net"
                , label = text "Ananth"
                }
            , text "."
            ]
        , Element.row []
            [ text "View the source at "
            , Element.newTabLink
                linkAttrs
                { url = "https://github.com/ananthb/dhcp-121"
                , label = text "github.com/ananthb/dhcp-121"
                }
            , text "."
            ]
        , Element.row []
            [ text "Inspired by the Javascript version at "
            , Element.newTabLink
                linkAttrs
                { url = "https://www.medo64.com/2018/01/configuring-classless-static-route-option/"
                , label = text "medo64.com"
                }
            , text "."
            ]
        ]
