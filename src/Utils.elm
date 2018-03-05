module Utils exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import AppStyle exposing (AppStyle(..))


loadingIndicator : Element AppStyle variation msg
loadingIndicator =
    screen <| decorativeImage NoStyle [ center, verticalCenter ] { src = "loading.gif" }
