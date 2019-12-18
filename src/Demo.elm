module Demo exposing (main)

import Browser
import Email
import Heading
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import MultiSelectField
import RadioField
import SelectField
import SliderField
import TextField


type alias Model =
    { email : TextField.Attributes
    , state : SelectField.Attributes
    , hotdog : RadioField.Attributes
    , food : RadioField.Attributes
    , soda : MultiSelectField.Attributes
    }


type Msg
    = SetEmail TextField.Attributes
    | SetState SelectField.Attributes
    | SetHotDog RadioField.Attributes
    | SetFood RadioField.Attributes
    | SetSoda MultiSelectField.Attributes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEmail attr ->
            ( { model | email = attr }, Cmd.none )

        SetState attr ->
            ( { model | state = attr }, Cmd.none )

        SetHotDog attr ->
            ( { model | hotdog = attr }, Cmd.none )

        SetFood attr ->
            ( { model | food = attr }, Cmd.none )

        SetSoda attr ->
            ( { model | soda = attr }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ TextField.view SetEmail model.email
            , SelectField.view SetState model.state
            , RadioField.view SetHotDog model.hotdog
            , RadioField.view SetFood model.food
            , MultiSelectField.view SetSoda model.soda
            ]
        ]


emailValidator : String -> Result String String
emailValidator s =
    Email.parse s
        |> Result.mapError (always "invalid email")
        |> Result.map Email.toString


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { email =
            { key = "email"
            , icon = TextField.WithIcon "fas fa-envelope"
            , label = "Email"
            , placeholder = "Email Address"
            , validator = emailValidator
            , value = TextField.EmptyText
            }
      , state =
            { key = "state"
            , label = "State"
            , placeholder = Just "-- State --"
            , value = SelectField.EmptySelect
            , options = usStates
            }
      , hotdog =
            { key = "hotdog"
            , label = "Is a hot dog a sandwich?"
            , value = RadioField.Empty
            , options =
                [ RadioField.SelectOption "Yes" "True"
                , RadioField.SelectOption "No" "False"
                ]
            }
      , food =
            { key = "fav_food"
            , label = "What is your favorite animal?"
            , value = RadioField.Empty
            , options =
                [ RadioField.SelectOption "Dog" "dog"
                , RadioField.SelectOption "Cat" "cat"
                , RadioField.SelectOption "Horse" "horse"
                , RadioField.OtherOption ""
                ]
            }
      , soda =
            { key = "soda"
            , label = "Which of the following do you like?"
            , value = MultiSelectField.Empty
            , options =
                List.map (\s -> MultiSelectField.SelectOption s s)
                    [ "Coke"
                    , "Pepsi"
                    , "Diet Coke"
                    , "Mountain Dew"
                    , "Dr Pepper"
                    , "Sprite"
                    , "Diet Pepsi"
                    , "Fanta"
                    ]
                    ++ [ MultiSelectField.OtherOption ""
                       ]
            }
      }
    , Cmd.none
    )


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


usStates : List ( String, String )
usStates =
    [ ( "Alabama", "AL" )
    , ( "Alaska", "AK" )
    , ( "Arizona", "AZ" )
    , ( "Arkansas", "AR" )
    , ( "California", "CA" )
    , ( "Colorado", "CO" )
    , ( "Connecticut", "CT" )
    , ( "Delaware", "DE" )
    , ( "District of Columbia", "DC" )
    , ( "Florida", "FL" )
    , ( "Georgia", "GA" )
    , ( "Hawaii", "HI" )
    , ( "Idaho", "ID" )
    , ( "Illinois", "IL" )
    , ( "Indiana", "IN" )
    , ( "Iowa", "IA" )
    , ( "Kansas", "KS" )
    , ( "Kentucky", "KY" )
    , ( "Louisiana", "LA" )
    , ( "Maine", "ME" )
    , ( "Maryland", "MD" )
    , ( "Massachusetts", "MA" )
    , ( "Michigan", "MI" )
    , ( "Minnesota", "MN" )
    , ( "Mississippi", "MS" )
    , ( "Missouri", "MO" )
    , ( "Montana", "MT" )
    , ( "Nebraska", "NE" )
    , ( "Nevada", "NV" )
    , ( "New Hampshire", "NH" )
    , ( "New Jersey", "NJ" )
    , ( "New Mexico", "NM" )
    , ( "New York", "NY" )
    , ( "North Carolina", "NC" )
    , ( "North Dakota", "ND" )
    , ( "Ohio", "OH" )
    , ( "Oklahoma", "OK" )
    , ( "Oregon", "OR" )
    , ( "Pennsylvania", "PA" )
    , ( "Rhode Island", "RI" )
    , ( "South Carolina", "SC" )
    , ( "South Dakota", "SD" )
    , ( "Tennessee", "TN" )
    , ( "Texas", "TX" )
    , ( "Utah", "UT" )
    , ( "Vermont", "VT" )
    , ( "Virginia", "VA" )
    , ( "Washington", "WA" )
    , ( "West virginia", "WV" )
    , ( "Wisconsin", "WI" )
    , ( "Wyoming", "WY" )
    , ( "American Samoa", "AS" )
    , ( "Guam", "GU" )
    , ( "Northern Mariana Islands", "MP" )
    , ( "Puerto Rico", "PR" )
    , ( "U.S. Virgin Islands", "VI" )
    ]
