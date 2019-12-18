module TextField exposing (Attributes, Icon(..), Value(..), password, value, view)

{-| TextField - renders a text field and validates the value

@docs Attributes, Icon, Value, password, value, view

-}

import Html exposing (Html, div, i, input, label, p, span, text)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onInput)


{-| The model to store the state for the element
-}
type alias Attributes =
    { key : String
    , icon : Icon
    , label : String
    , placeholder : String
    , validator : String -> Result String String
    , value : Value
    }


{-| An optional icon for the text field.
-}
type Icon
    = NoIcon
    | WithIcon String


{-| The current value for the field
-}
type Value
    = EmptyText
    | TextValid String
    | TextInvalid String String


{-| Converts a multi select value into a list of selected string
-}
value : Value -> String
value val =
    case val of
        EmptyText ->
            ""

        TextValid s ->
            s

        TextInvalid s _ ->
            s


handleInput : Attributes -> (Attributes -> msg) -> String -> msg
handleInput attrs msg val =
    case attrs.validator val of
        Ok v ->
            msg <| { attrs | value = TextValid v }

        Err e ->
            msg <| { attrs | value = TextInvalid val e }


viewHelper : String -> (Attributes -> msg) -> Attributes -> Html msg
viewHelper inputType msgInput attrs =
    let
        inputIcon icon =
            case icon of
                WithIcon css ->
                    span [ class "icon is-small is-left" ]
                        [ i [ class css ] []
                        ]

                _ ->
                    text ""

        ( isErr, val ) =
            case attrs.value of
                TextInvalid v _ ->
                    ( True, v )

                TextValid v ->
                    ( False, v )

                EmptyText ->
                    ( False, "" )

        errIcon =
            if isErr then
                span [ class "icon is-small is-right" ]
                    [ i [ class "fas fa-exclamation-triangle" ] []
                    ]

            else
                text ""

        errMessage =
            case attrs.value of
                TextInvalid _ e ->
                    p [ class "help is-danger" ] [ text e ]

                TextValid _ ->
                    text ""

                EmptyText ->
                    text ""
    in
    div [ class "field", class attrs.key ]
        [ if String.isEmpty attrs.label then
            text ""

          else
            label [ class "label" ] [ text attrs.label ]
        , div
            [ classList
                [ ( "control", True )
                , ( "has-icons-left", attrs.icon /= NoIcon )
                , ( "has-icons-right", isErr )
                ]
            ]
            [ input
                [ classList [ ( "input", True ), ( "is-danger", isErr ) ]
                , type_ inputType
                , placeholder attrs.placeholder
                , Html.Attributes.value val
                , onInput (handleInput attrs msgInput)
                ]
                []
            , inputIcon attrs.icon
            , errIcon
            ]
        , errMessage
        ]


{-| Renders the text field
-}
view : (Attributes -> msg) -> Attributes -> Html msg
view msgInput attrs =
    viewHelper "text" msgInput attrs


{-| Renders the special password text field
-}
password : (Attributes -> msg) -> Attributes -> Html msg
password msgInput attrs =
    viewHelper "password" msgInput attrs
