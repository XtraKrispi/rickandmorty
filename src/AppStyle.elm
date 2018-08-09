module AppStyle exposing (..)

import Color
import Style exposing (..)
import Style.Background
import Style.Border as Border
import Style.Color as SColor
import Style.Font as Font exposing (typeface, underline)
import Style.Shadow as Shadow
import Style.Transition as Transition


type AppStyle
    = NoStyle
    | GlobalStyle
    | NavBar NavBarStyle
    | PageHeaderStyle
    | PageStyle
    | Character CharacterStyle
    | Modal ModalStyle
    | PageOverlay
    | Divider
    | AppLink
    | CloseButton
    | IconStyle


type NavBarStyle
    = MainBar
    | Title
    | Link


type ModalStyle
    = Content


type CharacterStyle
    = Name
    | Container
    | Pager
    | DetailTitle
    | Image
    | DetailView
    | Label


type Variation
    = Selected
    | Hover
    | Active



-- Currently only one shadow works here, as they don't stack
-- There is a PR that will allow this to work in the future


customTextGlow : List (Property class variation)
customTextGlow =
    [ Shadow.text { offset = ( 0, 0 ), blur = 10, color = Color.white }
    ]



{- text-shadow: 0 0 10px rgba(255,255,255,1) , 0 0 20px rgba(255,255,255,1) , 0 0 30px rgba(255,255,255,1) , 0 0 40px #00ffff , 0 0 70px #00ffff , 0 0 80px #00ffff , 0 0 100px #00ffff -}


font : Property class variation
font =
    typeface
        [ Font.importUrl
            { url = "https://fonts.googleapis.com/css?family=Joti+One"
            , name = "Joti One"
            }
        ]


stylesheet : StyleSheet AppStyle Variation
stylesheet =
    styleSheet
        [ style GlobalStyle
            [ font
            ]
        , style PageStyle
            [ Style.Background.gradient 0 [ Style.Background.step Color.blue, Style.Background.step Color.green ]
            ]
        , style PageHeaderStyle
            [ Font.size 60
            , Shadow.text { offset = ( 0, 0 ), blur = 4, color = Color.red }
            , Font.center
            ]
        , style NoStyle []
        , style (NavBar MainBar)
            [ font
            , SColor.background Color.black
            , SColor.text <| Color.rgb 0 176 200
            ]
        , style (NavBar Title)
            [ Font.size 24 ]
        , style (NavBar Link)
            [ cursor "pointer"
            , hover <|
                customTextGlow
            , variation Selected
                customTextGlow
            ]
        , style (Character Name)
            [ SColor.text Color.white
            , prop "white-space" "nowrap"
            , prop "overflow" "hidden"
            , prop "text-overflow" "ellipsis"
            ]
        , style (Character Container)
            [ SColor.background Color.black
            , Transition.all
            , hover
                [ Shadow.glow Color.white 10
                , cursor "pointer"
                , scale 1.1 1.1 1.1
                , prop "z-index" "9"
                ]
            ]
        , style (Character Pager)
            [ font
            , SColor.background <| Color.rgba 100 100 100 0.5
            , cursor "pointer"
            , Transition.all
            , hover
                [ SColor.background Color.white
                ]
            ]
        , style (Modal Content)
            [ SColor.background Color.white ]
        , style PageOverlay
            [ SColor.background <| Color.rgba 0 0 0 0.8
            ]
        , style (Character Image)
            []
        , style (Character DetailView)
            [ font, SColor.background Color.black, SColor.text Color.white ]
        , style (Character DetailTitle)
            [ Font.size 24 ]
        , style Divider
            [ SColor.border <| Color.rgb 0 176 200
            , Border.all 1
            ]
        , style (Character Label) [ prop "word-wrap" "break-word" ]
        , style AppLink [ cursor "pointer", underline ]
        , style CloseButton [ cursor "pointer" ]
        , style IconStyle [ typeface [ Font.font "FontAwesome" ] ]
        ]
