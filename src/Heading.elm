module Heading exposing (Heading, view)

import Html exposing (Html, p, text)
import Html.Attributes exposing (class)


type alias Heading =
    String


view : Heading -> Html msg
view str =
    p [ class "form-label" ] [ text str ]
