module RadioField exposing (Attributes, Option(..), Value(..), isValueSelected, stringToOption, value, view)

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List


type Value
    = Empty
    | Selected Option


type Option
    = SelectOption String String
    | OtherOption String


type alias Attributes =
    { key : String
    , label : String
    , options : List Option
    , value : Value
    }


stringToOption : String -> Option
stringToOption s =
    SelectOption s s


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


value : Value -> String
value val =
    case val of
        Selected (SelectOption _ v) ->
            v

        Selected (OtherOption v) ->
            v

        Empty ->
            ""


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
