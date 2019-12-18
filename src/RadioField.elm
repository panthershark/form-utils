module RadioField exposing (Attributes, isValueSelected, stringToOption, value, view, Option(..), Value(..))

{-| RadioField - renders a list of options and allows user to select a single value, including other.

@docs Attributes, isValueSelected, stringToOption, value, view, Option, Value

-}

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List


{-| The current value for the field
-}
type Value
    = Empty
    | Selected Option


{-| An option that is included in the list of possible selections
-}
type Option
    = SelectOption String String
    | OtherOption String


{-| The model to store the state for the element
-}
type alias Attributes =
    { key : String
    , label : String
    , options : List Option
    , value : Value
    }


{-| Helper for converting a string to an Option

        List.map stringToOption ["dog", "cat"] == [ SelectOption "dog" dog", SelectOption "cat" cat" ]

-}
stringToOption : String -> Option
stringToOption s =
    SelectOption s s


{-| Returns true is the selected value matches a specific string. Useful for unpacking the internal state.
-}
isValueSelected : List String -> Value -> Bool
isValueSelected vals fieldval =
    case fieldval of
        Selected (SelectOption _ v) ->
            List.find (\x -> x == v) vals
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        Selected (OtherOption v) ->
            List.find (\x -> x == v) vals
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        Empty ->
            False


{-| Converts a multi select value into the selected string
-}
value : Value -> String
value val =
    case val of
        Selected (SelectOption _ v) ->
            v

        Selected (OtherOption v) ->
            v

        Empty ->
            ""


{-| Renders the radio
-}
view : (Attributes -> msg) -> Attributes -> Html msg
view msgChange attrs =
    let
        viewOption : String -> Option -> Html msg
        viewOption key opt =
            div [ class "container" ]
                [ label [ class "radio" ]
                    [ input
                        [ type_ "radio"
                        , name key
                        , onClick (msgChange { attrs | value = Selected opt })
                        , checked
                            (case ( attrs.value, opt ) of
                                ( Selected (OtherOption _), OtherOption _ ) ->
                                    True

                                ( Selected (SelectOption _ a), SelectOption _ b ) ->
                                    a == b

                                ( _, _ ) ->
                                    False
                            )
                        ]
                        []
                    , text
                        (case opt of
                            OtherOption _ ->
                                "Other"

                            SelectOption b _ ->
                                b
                        )
                    ]
                ]

        otherField =
            case attrs.value of
                Selected (OtherOption otherVal) ->
                    input
                        [ class "input other-field"
                        , type_ "text"
                        , Html.Attributes.value otherVal
                        , placeholder "Other"
                        , onInput (\s -> msgChange { attrs | value = Selected <| OtherOption s })
                        ]
                        []

                _ ->
                    text ""
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text attrs.label ]
        , div [ class "control" ]
            (List.map (viewOption attrs.key) attrs.options)
        , otherField
        ]
