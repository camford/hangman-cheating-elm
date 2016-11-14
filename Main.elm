import Html.App exposing (beginnerProgram, program)
import Html exposing (Html, div, text ,input)
import Html.Attributes exposing (placeholder, maxlength, size)
import Html.Events exposing (onInput)
import String exposing (toInt, toList, fromList)
import Char exposing (toLower)
import List exposing (head, length, reverse, take, append, repeat)
import Maybe exposing (map)
import Set exposing (Set, toList, fromList, empty)

main : Program Never
main = Html.App.program
    { init = model ! []
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


---------- Types ----------

type alias Model =
    { length : Int
    , secret : List Char
    , guesses : Set Char
    , wordlist : List String
    }

type Msg
    = ChangeLength String
    | ChangeLetter Int String
    | AddGuess String


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
            [ text "Guessed letters: "
            , input
                [ placeholder "Enter guessed letters"
                , onInput AddGuess
                ]
                []
            ]
        , div
            []
            [ text "Guess: "
            , text <| String.fromList model.secret
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
    , secret = []
    , guesses = Set.empty
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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLength s ->
            case (toInt s) of
                Ok i ->
                    { model
                        | length = i
                        , secret = repeat i '.'
                    } ! []
                Err _ ->
                    model ! []
        ChangeLetter i s ->
            let
                updated = updateGuess
                              i
                              s
                              model.secret
            in
                { model | secret = updated } ! []
        AddGuess s ->
            { model |
                guesses = model.guesses
                              |> Set.toList
                              |> List.append (String.toList s)
                              |> Set.fromList
            } ! []
                      

updateGuess : Int -> String -> List Char -> List Char
updateGuess i chars secret =
    let
        char = chars
                |> String.toList
                |> List.head
                |> Maybe.map Char.toLower
    in
        case char of
            Nothing ->
                secret
            Just c ->
                let
                    l = length secret
                    prefix = secret
                                 |> take i
                    suffix = secret
                                 |> reverse
                                 |> take (l - i - 1)
                                 |> reverse
                in
                    append prefix (c :: suffix)
