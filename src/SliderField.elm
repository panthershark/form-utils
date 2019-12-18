module SliderField exposing (Attributes, Option, Value(..), isValueSelected, stringToOption, value, view)

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onClick)
import List.Extra as List


type Value
    = Empty
    | Selected Option


type alias Option =
    ( String, String )


type alias Attributes =
    { key : String
    , label : String
    , options : List Option
    , value : Value
    }


stringToOption : String -> Option
stringToOption s =
    ( s, s )


isValueSelected : List String -> Value -> Bool
isValueSelected searchVals fieldVal =
    case fieldVal of
        Selected ( _, v ) ->
            List.find (\x -> x == v) searchVals
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        Empty ->
            False


value : Value -> String
value val =
    case val of
        Selected ( _, v ) ->
            v

        Empty ->
            ""


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
