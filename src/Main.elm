module Main exposing (main)

import Array exposing (Array)
import Base
import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, fill, padding, px, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
    , routes : Array StaticRoute
    , option121 : Maybe String
    }


newStaticRoute : StaticRoute
newStaticRoute =
    { destination = Invalid "" "Enter a classless destination network"
    , router = Invalid "" "Enter a valid IP address"
    }


newStaticRoutes : Array StaticRoute
newStaticRoutes =
    Array.fromList [ { newStaticRoute | destination = Valid "0.0.0.0/0" "00" } ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    -- TODO: Parse the URL and set the routes
    ( { key = key
      , url = url
      , routes = newStaticRoutes
      , option121 = Nothing
      }
    , Nav.pushUrl key "#"
    )



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
            ( Just opt, Nav.pushUrl key <| "#" ++ opt )

        Nothing ->
            ( Nothing, Nav.pushUrl key "#" )



{- Refer this for the function below

   function parseNetwork(ip) { //null if empty; "" if error; otherwise hex
     if (ip == "") { return null; }

     var subnetSize = 32;
     var parts = ip.split('/');
     if (parts.length == 2) {
       if (/^[0-9]+$/.test(parts[1].trim())) {
         subnetSize  = parseInt(parts[1], 10);
         if ((subnetSize < 1) || (subnetSize > 32)) { return ""; }
       } else {
         return "";
       }
     } else if (parts.length > 2) {
       return "";
     }

     var result = ((subnetSize < 16) ? "0" : "") + subnetSize.toString(16);

     var octetCount = parseInt((subnetSize + 7) / 8);

     var octets = parts[0].trim().split('.');
     if (octets.length != 4) {  return ""; }

     for(var i=0; i<4; i++) {
       if (/^[0-9]+$/.test(octets[i])) {
         var octetValue = parseInt(octets[i], 10);
         if ((octetValue < 0) || (octetValue > 255)) { return ""; }
         if ((subnetSize > 0) && (subnetSize < 32)) {
           var octetValueRem = (octetValue << subnetSize) & 0xFF; //remove network bits
           if (octetValueRem > 0) { return "" ; } //invalid subnet bitmask - some network bits remaining
           result += ((octetValue < 16) ? "0" : "") + octetValue.toString(16);
         } else if (subnetSize < 32) { //check even though we don't need them
           if (octetValue > 0) { return ""; } //way too many numbers
         } else if (subnetSize == 32) {
           result += ((octetValue < 16) ? "0" : "") + octetValue.toString(16);
         }
         subnetSize -= 8;
       } else {
         return "";
       }
     }

     return result.toUpperCase();
   }

-}


{-| TODO: Parse this as a subnet instead of a plain IP

We gotta take the subnet into account.

So 10.10.10.0/24 is valid but 10.10.10.1/24 is not.let

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
    Sub.none



-- VIEW


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
            [ viewOption121 model
            , viewRoutesTable model
            , viewButtons
            , viewFooter
            ]
            |> Element.layout []
        ]
    }


viewOption121 : Model -> Element Msg
viewOption121 model =
    let
        option121 =
            case model.option121 of
                Just val ->
                    text val

                Nothing ->
                    text "Some routes are invalid"
    in
    [ option121 ]
        |> Element.column [ centerX, padding 20, Element.spacing 20 ]


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
                Valid val _ ->
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


viewButtons : Element Msg
viewButtons =
    Element.row
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


viewFooter : Element Msg
viewFooter =
    let
        linkAttrs =
            [ Font.color <| rgb255 2 48 32
            , Font.underline
            ]
    in
    Element.column
        [ Region.footer
        , Element.spacing 5
        , Element.padding 20
        , Font.size 14
        , Font.color (rgb255 128 128 128)
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
