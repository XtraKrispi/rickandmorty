module Main exposing (..)

import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Page.Characters as CharactersPage
import Route exposing (Route)
import AppStyle exposing (..)
import Mouse exposing (moves)


---- MODEL ----


type alias Model =
    { baseUrl : String
    , currentRoute : Route.Route
    , currentPage : Page
    , mousePosition : Mouse.Position
    }


type alias RouteModel r =
    { r | currentRoute : Route.Route }


type alias MousePositionModel r =
    { r | mousePosition : Mouse.Position }


type Page
    = Characters CharactersPage.Model
    | Locations
    | Episodes
    | NotFound


init : String -> ( Model, Cmd Msg )
init baseUrl =
    let
        ( charactersPageModel, charactersPageCmd ) =
            CharactersPage.init baseUrl
    in
        ( { baseUrl = baseUrl
          , currentPage = Characters charactersPageModel
          , currentRoute = Route.Characters
          , mousePosition = { x = 0, y = 0 }
          }
        , Cmd.map CharactersPageMsg charactersPageCmd
        )



---- UPDATE ----


type Msg
    = ChangePage Route.Route
    | CharactersPageMsg CharactersPage.Msg
    | MouseMove Mouse.Position


updateRoute : Route -> Model -> Model
updateRoute r m =
    { m | currentRoute = r }


updateMousePosition : Mouse.Position -> MousePositionModel r -> MousePositionModel r
updateMousePosition pos model =
    { model | mousePosition = pos }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentPage ) of
        ( ChangePage route, _ ) ->
            case route of
                Route.Characters ->
                    let
                        ( m, cmd ) =
                            CharactersPage.init model.baseUrl
                    in
                        updateRoute route
                            { model
                                | currentPage = Characters m
                            }
                            ! [ Cmd.map CharactersPageMsg cmd ]

                _ ->
                    updateRoute route { model | currentPage = NotFound } ! []

        ( CharactersPageMsg msg, Characters pageModel ) ->
            let
                ( newModel, cmd ) =
                    CharactersPage.update model.baseUrl msg pageModel
            in
                { model | currentPage = Characters newModel } ! [ Cmd.map CharactersPageMsg cmd ]

        _ ->
            model ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pageView =
            case model.currentPage of
                Characters modelCharactersPage ->
                    CharactersPage.view modelCharactersPage
                        |> Element.map CharactersPageMsg

                Locations ->
                    el NoStyle [] (text "Nope")

                Episodes ->
                    el NoStyle [] (text "Nope")

                NotFound ->
                    el NoStyle [] (text "Not Found!")
    in
        viewport stylesheet <|
            column GlobalStyle
                [ height fill
                ]
                [ navbar model.currentRoute
                , (el PageStyle [ height fill, paddingXY 200 100 ] pageView)
                ]


navbar : Route -> Element AppStyle Variation Msg
navbar currentRoute =
    let
        link route t =
            el (NavBar Link)
                ([ vary Selected (currentRoute == route)
                 ]
                    ++ if currentRoute == route then
                        []
                       else
                        [ onClick <| ChangePage route ]
                )
                (text t)
    in
        screen <|
            el (NavBar MainBar)
                [ height (px 50), width fill ]
                (row NoStyle
                    [ height (percent 100), spacing 40, verticalCenter ]
                    [ el (NavBar Title)
                        [ paddingLeft 20
                        , paddingRight 20
                        ]
                        (image NoStyle [ height (px 50) ] { src = "Rick_and_Morty_logo.png", caption = "Logo" })
                    , link Route.Characters "Characters"
                    , link Route.Locations "Locations"
                    , link Route.Episodes "Episodes"
                    ]
                )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentRoute of
        Route.Characters ->
            Sub.map CharactersPageMsg CharactersPage.subscriptions

        _ ->
            Sub.none


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
