module SliderField exposing (Attributes, isValueSelected, stringToOption, value, view, Option, Value(..))

{-| SliderField - In future, this will be a slider. For now, it is implemented as a radio

@docs Attributes, isValueSelected, stringToOption, value, view, Option, Value

-}

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onClick)
import List.Extra as List


{-| The current value for the field
-}
type Value
    = Empty
    | Selected Option


{-| An option that is included in the list of possible selections
-}
type alias Option =
    ( String, String )


{-| The model to store the state for the element
-}
type alias Attributes =
    { key : String
    , label : String
    , options : List Option
    , value : Value
    }


{-| Helper for converting a string to an Option

        List.map stringToOption ["dog", "cat"] == [ ("dog" dog"), ("cat" cat") ]

-}
stringToOption : String -> Option
stringToOption s =
    ( s, s )


{-| Returns true is the selected value matches a specific string. Useful for unpacking the internal state.
-}
isValueSelected : List String -> Value -> Bool
isValueSelected searchVals fieldVal =
    case fieldVal of
        Selected ( _, v ) ->
            List.find (\x -> x == v) searchVals
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        Empty ->
            False


{-| Converts a multi select value into the selected string
-}
value : Value -> String
value val =
    case val of
        Selected ( _, v ) ->
            v

        Empty ->
            ""


{-| Renders the slider
-}
view : (Attributes -> msg) -> Attributes -> Html msg
view msgChange attrs =
    let
        viewOption : String -> Option -> Html msg
        viewOption key opt =
            label [ class "radio" ]
                [ input
                    [ type_ "radio"
                    , name key
                    , onClick (msgChange { attrs | value = Selected opt })
                    , checked
                        (case ( attrs.value, opt ) of
                            ( Selected ( _, a ), ( _, b ) ) ->
                                a == b

                            ( _, _ ) ->
                                False
                        )
                    ]
                    []
                , text <| Tuple.first opt
                ]
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text attrs.label ]
        , div [ class "control" ]
            (List.map (viewOption attrs.key) attrs.options)
        ]
