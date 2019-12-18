module SelectField exposing (Attributes, Value(..), stringToOption, value, view)

import Html exposing (Html, div, label, option, select, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode


type Value
    = EmptySelect
    | Selected SelectOption


type alias SelectOption =
    ( String, String )


type alias Attributes =
    { key : String
    , label : String
    , placeholder : Maybe String
    , options : List SelectOption
    , value : Value
    }


stringToOption : String -> SelectOption
stringToOption s =
    ( s, s )


value : Value -> String
value val =
    case val of
        EmptySelect ->
            ""

        Selected ( _, v ) ->
            v


view : (Attributes -> msg) -> Attributes -> Html msg
view msgChange attrs =
    let
        viewOption ( txt, val ) =
            option [ Html.Attributes.value val ] [ text txt ]

        handleInput sel =
            let
                opt =
                    List.filter (\( _, val ) -> val == sel) attrs.options
                        |> List.head
            in
            case opt of
                Just selOption ->
                    msgChange { attrs | value = Selected selOption }

                Nothing ->
                    msgChange { attrs | value = EmptySelect }
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text attrs.label ]
        , div [ class "select" ]
            [ select [ on "change" (Decode.map handleInput targetValue) ] (viewOption ( Maybe.withDefault "" attrs.placeholder, "" ) :: List.map viewOption attrs.options)
            ]
        ]
