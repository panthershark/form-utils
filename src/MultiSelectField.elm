module MultiSelectField exposing (Attributes, Option(..), Value(..), value, view)

{-| MultiSelectField - renders a list of options and allows user to select multiple values including other.

@docs Attributes, Option, Value, value, view

-}

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


{-| The current value for the field
-}
type Value
    = Empty
    | Selected (List Option)


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


{-| Converts a multi select value into a list of selected string
-}
value : Value -> List String
value val =
    case val of
        Empty ->
            []

        Selected sel ->
            List.map
                (\opt ->
                    case opt of
                        SelectOption _ v ->
                            v

                        OtherOption v ->
                            v
                )
                sel


isEqual : Option -> Option -> Bool
isEqual val opt =
    case ( val, opt ) of
        ( OtherOption _, OtherOption _ ) ->
            True

        ( SelectOption _ a, SelectOption _ b ) ->
            a == b

        ( _, _ ) ->
            False


isSelected : Option -> List Option -> Bool
isSelected val opts =
    List.filter (isEqual val) opts
        |> List.head
        |> Maybe.map (always True)
        |> Maybe.withDefault False


findOtherText : List Option -> Maybe String
findOtherText vals =
    List.filterMap
        (\v ->
            case v of
                OtherOption str ->
                    Just str

                _ ->
                    Nothing
        )
        vals
        |> List.head


isOther : Option -> Bool
isOther opt =
    case opt of
        OtherOption _ ->
            True

        _ ->
            False


replaceOther : String -> Option -> List Option -> List Option
replaceOther str opt vals =
    if isOther opt then
        OtherOption str :: vals

    else
        opt :: vals


{-| Renders the multi select
-}
view : (Attributes -> msg) -> Attributes -> Html msg
view msgChange attrs =
    let
        viewOption : Option -> Html msg
        viewOption opt =
            let
                txt =
                    case opt of
                        OtherOption _ ->
                            "Other"

                        SelectOption t _ ->
                            t

                isChecked =
                    case attrs.value of
                        Selected vals ->
                            isSelected opt vals

                        _ ->
                            False
            in
            div [ class "container" ]
                [ label [ class "checkbox" ]
                    [ input
                        [ type_ "checkbox"
                        , checked isChecked
                        , case ( isChecked, attrs.value ) of
                            ( True, Selected vals ) ->
                                onClick (msgChange { attrs | value = Selected <| List.filter (\x -> not <| isEqual opt x) vals })

                            ( False, Empty ) ->
                                onClick (msgChange { attrs | value = Selected [ opt ] })

                            ( False, Selected vals ) ->
                                onClick (msgChange { attrs | value = Selected <| opt :: vals })

                            ( _, _ ) ->
                                onClick (msgChange attrs)
                        ]
                        []
                    , text txt
                    ]
                ]

        otherField =
            case attrs.value of
                Selected vals ->
                    case findOtherText vals of
                        Just otherVal ->
                            input
                                [ class "input other-field"
                                , type_ "text"
                                , Html.Attributes.value otherVal
                                , placeholder "Other"
                                , onInput (\s -> msgChange { attrs | value = Selected <| List.foldr (replaceOther s) [] vals })
                                ]
                                []

                        _ ->
                            text ""

                Empty ->
                    text ""
    in
    div [ class "field" ]
        [ label [ class "label" ] [ text attrs.label ]
        , div [] (List.map viewOption attrs.options)
        , otherField
        ]
