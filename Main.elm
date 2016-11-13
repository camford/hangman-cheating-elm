import Html.App exposing (beginnerProgram)
import Html exposing (Html, div, text ,input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)

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
            [ input
                [ placeholder (toString model.length)
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

update : a -> Model -> Model
update msg model = model
