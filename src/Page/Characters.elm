module Page.Characters exposing (Model, init, Msg, update, view, subscriptions)

import AppStyle exposing (..)
import Debounce
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Http exposing (get, send)
import Json.Decode as Json exposing (Decoder, andThen, at, int, list, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Keyboard exposing (downs)
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (second)
import Utils exposing (loadingIndicator)
import Task


type Status
    = Alive
    | Dead
    | UnknownStatus


type Gender
    = Female
    | Male
    | Genderless
    | UnknownGender


type alias LocationUrl =
    { name : String, url : String }


type alias PageInfo r =
    { r | nextPage : Maybe String, previousPage : Maybe String }


type alias Character =
    { id : Int
    , name : String
    , status : Status
    , species : String
    , characterType : String
    , gender : Gender
    , origin : LocationUrl
    , lastKnownLocation : LocationUrl
    , imageUrl : String
    , episodeUrls : List String
    , url : String
    , episodes : WebData (List Episode)
    }


type alias Episode =
    { id : Int
    , name : String
    , airDate : String
    , episode : String
    , characterUrls : List String
    , url : String
    }


type Model
    = Model
        { characters : WebData (List Character)
        , nextPage : Maybe String
        , previousPage : Maybe String
        , searchText : String
        , debounce : Debounce.Debounce String
        , selectedCharacter : Maybe Int
        }


type Msg
    = RequestDataSucceeded { characters : List Character, nextPage : Maybe String, previousPage : Maybe String }
    | RequestDataFailed Http.Error
    | NextPage
    | PreviousPage
    | Search String
    | DebounceMsg Debounce.Msg
    | CharacterSelected Character
    | KeyPressed Keyboard.KeyCode
    | HideModal
    | LoadEpisodes Character
    | CharacterEpisodesSucceeded Character (List Episode)
    | CharacterEpisodesFailed Character Http.Error


charactersDecoder : Decoder (List Character)
charactersDecoder =
    at [ "results" ] (list characterDecoder)


infoDecoder : Decoder ( Maybe String, Maybe String )
infoDecoder =
    let
        mapEmptyToNothing =
            (Json.map
                (\s ->
                    if s == "" then
                        Nothing
                    else
                        Just s
                )
            )
                string
    in
        at [ "info" ]
            (decode (,)
                |> required "next" mapEmptyToNothing
                |> required "prev" mapEmptyToNothing
            )


requestData : String -> Cmd Msg
requestData url =
    get url (map2 (\( n, p ) c -> { characters = c, nextPage = n, previousPage = p }) infoDecoder charactersDecoder)
        |> send
            (\result ->
                case result of
                    Ok characters ->
                        RequestDataSucceeded characters

                    Err err ->
                        RequestDataFailed err
            )


statusFromString : String -> Status
statusFromString s =
    case String.toLower s of
        "alive" ->
            Alive

        "dead" ->
            Dead

        _ ->
            UnknownStatus


genderFromString : String -> Gender
genderFromString s =
    case String.toLower s of
        "male" ->
            Male

        "female" ->
            Female

        "genderless" ->
            Genderless

        _ ->
            UnknownGender


locationUrlDecoder : Decoder LocationUrl
locationUrlDecoder =
    decode LocationUrl
        |> required "name" string
        |> required "url" string


characterDecoder : Decoder Character
characterDecoder =
    decode Character
        |> required "id" int
        |> required "name" string
        |> required "status" (string |> andThen (succeed << statusFromString))
        |> required "species" string
        |> required "type" string
        |> required "gender" (string |> andThen (succeed << genderFromString))
        |> required "origin" locationUrlDecoder
        |> required "location" locationUrlDecoder
        |> required "image" string
        |> required "episode" (list string)
        |> required "url" string
        |> hardcoded NotAsked


episodeDecoder : Decoder Episode
episodeDecoder =
    decode Episode
        |> required "id" int
        |> required "name" string
        |> required "air_date" string
        |> required "episode" string
        |> required "characters" (list string)
        |> required "url" string


init : String -> ( Model, Cmd Msg )
init baseUrl =
    ( Model
        { characters = Loading
        , nextPage = Nothing
        , previousPage = Nothing
        , searchText = ""
        , debounce = Debounce.init
        , selectedCharacter = Nothing
        }
    , requestData (baseUrl ++ "/character")
    )


subscriptions : Sub Msg
subscriptions =
    downs KeyPressed


hideModal : Model -> Model
hideModal (Model m) =
    Model { m | selectedCharacter = Nothing }


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update baseUrl msg (Model model) =
    case msg of
        RequestDataSucceeded { characters, nextPage, previousPage } ->
            Model { model | characters = Success characters, nextPage = nextPage, previousPage = previousPage } ! []

        RequestDataFailed err ->
            Model { model | characters = Failure err } ! []

        NextPage ->
            case model.nextPage of
                Just p ->
                    Model model ! [ requestData p ]

                Nothing ->
                    Model model ! []

        PreviousPage ->
            case model.previousPage of
                Just p ->
                    Model model ! [ requestData p ]

                Nothing ->
                    Model model ! []

        Search s ->
            let
                -- Push your values here.
                ( debounce, cmd ) =
                    Debounce.push debounceConfig s model.debounce
            in
                Model
                    { model
                        | searchText = s
                        , debounce = debounce
                    }
                    ! [ cmd ]

        DebounceMsg msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast <| search baseUrl)
                        msg
                        model.debounce
            in
                Model { model | debounce = debounce } ! [ cmd ]

        CharacterSelected character ->
            Model { model | selectedCharacter = Just character.id } ! []

        KeyPressed key ->
            case key of
                27 ->
                    hideModal (Model model) ! []

                _ ->
                    Model model ! []

        HideModal ->
            hideModal (Model model) ! []

        LoadEpisodes character ->
            case model.characters of
                Success characters ->
                    let
                        newCharacters =
                            List.map
                                (\a ->
                                    if a.id == character.id then
                                        { character | episodes = Loading }
                                    else
                                        a
                                )
                    in
                        Model
                            { model
                                | characters = Success <| newCharacters characters
                            }
                            ! [ loadEpisodes character ]

                _ ->
                    Model model ! []

        CharacterEpisodesSucceeded character episodes ->
            case model.characters of
                Success characters ->
                    let
                        newCharacters =
                            List.map
                                (\a ->
                                    if a.id == character.id then
                                        { character | episodes = Success episodes }
                                    else
                                        a
                                )
                    in
                        Model
                            { model
                                | characters = Success <| newCharacters characters
                            }
                            ! []

                _ ->
                    Model model ! []

        CharacterEpisodesFailed character error ->
            case model.characters of
                Success characters ->
                    let
                        newCharacters =
                            List.map
                                (\a ->
                                    if a.id == character.id then
                                        { character | episodes = Failure error }
                                    else
                                        a
                                )
                    in
                        Model
                            { model
                                | characters = Success <| newCharacters characters
                            }
                            ! []

                _ ->
                    Model model ! []


loadEpisodes : Character -> Cmd Msg
loadEpisodes character =
    character.episodeUrls
        |> List.map (flip get episodeDecoder)
        |> List.map Http.toTask
        |> Task.sequence
        |> Task.attempt
            (\result ->
                case result of
                    Ok data ->
                        CharacterEpisodesSucceeded character data

                    Err err ->
                        CharacterEpisodesFailed character err
            )


search : String -> String -> Cmd Msg
search baseUrl s =
    requestData (baseUrl ++ "/character/?name=" ++ s)



-- This defines how the debouncer should work.
-- Choose the strategy for your use case.


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (1 * second)
    , transform = DebounceMsg
    }


view : Model -> Element AppStyle Variation Msg
view (Model model) =
    let
        overlay =
            screen <|
                el
                    PageOverlay
                    [ width (percent 100), height (percent 100), onClick HideModal ]
                    empty
    in
        column NoStyle
            [ spacing 20 ]
            ([ el PageHeaderStyle [] (text "Characters")
             , Input.search NoStyle
                [ width (px 400), center, padding 10 ]
                { onChange = Search
                , value = model.searchText
                , label = Input.placeholder { text = "Search", label = Input.hiddenLabel "" }
                , options = []
                }
             , pageView model model.characters
             , pager model
             ]
                ++ case ( model.selectedCharacter, model.characters ) of
                    ( Nothing, _ ) ->
                        []

                    ( Just characterId, Success characters ) ->
                        case List.head <| List.filter ((==) characterId << .id) characters of
                            Just character ->
                                [ within [ overlay ] <|
                                    modal
                                        NoStyle
                                        [ width (px 1000)
                                        , center
                                        , verticalCenter
                                        ]
                                        (el (Modal Content)
                                            [ width (percent 100)
                                            , center
                                            , verticalCenter
                                            ]
                                         <|
                                            detailedCharacterView character
                                        )
                                ]

                            Nothing ->
                                []

                    _ ->
                        []
            )


pageView : PageInfo r -> WebData (List Character) -> Element AppStyle Variation Msg
pageView p data =
    case data of
        NotAsked ->
            el NoStyle [] (text "No data fetched")

        Loading ->
            loadingIndicator

        Success characters ->
            characters
                |> List.map characterView
                |> wrappedRow NoStyle [ spacing 10, center ]

        Failure err ->
            el NoStyle [] (text <| "Error: " ++ (toString err))


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


pager : PageInfo r -> Element AppStyle Variation Msg
pager pageInfo =
    row NoStyle
        []
        [ when (isJust pageInfo.previousPage) <|
            screen <|
                el (AppStyle.Character Pager)
                    [ verticalCenter
                    , alignLeft
                    , width (px 100)
                    , height (px 200)
                    , onClick PreviousPage
                    ]
                    (el NoStyle [ verticalCenter, center ] (text "<< Prev"))
        , when (isJust pageInfo.nextPage) <|
            screen <|
                el (AppStyle.Character Pager)
                    [ verticalCenter
                    , alignRight
                    , width (px 100)
                    , height (px 200)
                    , onClick NextPage
                    ]
                    (el NoStyle [ verticalCenter, center ] (text "Next >>"))
        ]


detailedCharacterView : Character -> Element AppStyle Variation Msg
detailedCharacterView character =
    let
        episodes =
            case character.episodes of
                Success eps ->
                    List.map (\e -> el NoStyle [] (text e.name)) eps

                Failure err ->
                    [ el NoStyle [] (text "Failed to load...") ]

                _ ->
                    []

        itemRow label value =
            row NoStyle
                [ spacing 40
                ]
                [ el (AppStyle.Character Label) [ width (px 200) ] (text label)
                , el NoStyle [] (text value)
                ]
    in
        row (AppStyle.Character DetailView)
            []
            [ el (AppStyle.Character Image)
                [ paddingTop 10, paddingLeft 10, paddingBottom 10 ]
                (image NoStyle [] { src = character.imageUrl, caption = character.name })
            , column NoStyle
                [ spacing 10, width fill, padding 10 ]
                [ row NoStyle
                    [ spread ]
                    [ h2 (AppStyle.Character DetailTitle) [ center ] (text character.name)
                    , el CloseButton [ onClick HideModal, alignRight ] (text "X")
                    ]
                , hairline Divider
                , itemRow "Gender:" (toString character.gender)
                , itemRow "Location:" character.lastKnownLocation.name
                , itemRow "Status:" (toString character.status)
                , itemRow "Species:" character.species
                , row NoStyle
                    [ spacing 40
                    ]
                    (List.concat
                        [ [ el (AppStyle.Character Label) [ width (px 200) ] (text "Episodes:") ]
                        , case character.episodes of
                            NotAsked ->
                                [ el AppLink [ onClick (LoadEpisodes character) ] (text "Load Episode Info") ]

                            Loading ->
                                [ el NoStyle [ onClick (LoadEpisodes character) ] (text "Loading...") ]

                            _ ->
                                []
                        ]
                    )
                , when (List.length episodes > 0) <|
                    column NoStyle
                        [ maxHeight (px 78), scrollbars ]
                        episodes
                ]
            ]


characterView : Character -> Element AppStyle Variation Msg
characterView character =
    column (AppStyle.Character Container)
        [ paddingTop 10, paddingLeft 10, paddingRight 10, width (px 320), onClick (CharacterSelected character) ]
        [ el NoStyle
            []
            (image NoStyle
                []
                { src = character.imageUrl
                , caption = character.name
                }
            )
        , wrappedRow NoStyle [] [ el (AppStyle.Character Name) [ center ] (text character.name) ]
        ]
