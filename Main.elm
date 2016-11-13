import Html.App exposing (beginnerProgram)
import Html exposing (Html, div, text ,input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import String exposing (toInt)

main : Program Never
main = Html.App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


---------- Types ----------

type alias Model =
    { length : Int
    , guess : String
    , wordlist : List String
    }

type Msg
    = ChangeLength String


---------- View ----------

view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ div
                []
                [ text "What's the length of the word?" ]
            , input
                [ placeholder "Enter a number"
                , onInput ChangeLength
                ]
                []
            ]
        ]


---------- State ----------

model : Model
model =
    { length = 0
    , guess = ""
    , wordlist = [ "boat"
                 , "moat"
                 , "coat"
                 , "card"
                 , "railing"
                 , "failing"
                 , "falling"
                 , "calling"
                 ]
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeLength s ->
            case (toInt s) of
                Ok i ->
                    { model | length = i }
                Err _ ->
                    model
