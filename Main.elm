import Html.App exposing (beginnerProgram)
import Html exposing (Html, div, text ,input)
import Html.Attributes exposing (placeholder, maxlength, size)
import Html.Events exposing (onInput)
import String exposing (toInt, toList, fromList)
import Char exposing (toLower)
import List exposing (head, length, reverse, take, append, repeat)
import Maybe exposing (map)

main : Program Never
main = Html.App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


---------- Types ----------

type alias Model =
    { length : Int
    , guess : List Char
    , wordlist : List String
    }

type Msg
    = ChangeLength String
    | ChangeLetter Int String


---------- View ----------

view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ div
                []
                [ text "What's the length of the word? "
                , input
                    [ placeholder "Enter a number"
                    , onInput ChangeLength
                    ]
                    []
                ]
            ]
        , div
            []
            ( text "Partial Solution: "
                :: viewLetters model )
        , div
            []
            [ text "Guess: "
            , text <| fromList model.guess
            ]
        ]


viewLetters : Model -> List (Html Msg)
viewLetters model =
    let
        letters = [1..model.length]
        createLetter i =
            input
                [ maxlength 1
                , size 1 
                , onInput <| ChangeLetter (i-1)
                ]
                []
    in
        List.map createLetter letters

    

---------- State ----------

model : Model
model =
    { length = 0
    , guess = []
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
                    { model | length = i
                            , guess = repeat i '.'
                    }
                Err _ ->
                    model
        ChangeLetter i s ->
            let
                updated = updateGuess
                              i
                              s
                              model.guess

            in
                { model | guess = updated }

updateGuess : Int -> String -> List Char -> List Char
updateGuess i chars guess =
    let
        char = chars
                |> String.toList
                |> List.head
                |> Maybe.map Char.toLower
    in
        case char of
            Nothing ->
                guess
            Just c ->
                let
                    l = length guess
                    prefix = guess
                                 |> take i
                    suffix = guess
                                 |> reverse
                                 |> take (l - i - 1)
                                 |> reverse
                in
                    append prefix (c :: suffix)
