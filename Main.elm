import Html.App exposing (beginnerProgram, program)
import Html exposing (Html, div, text ,input, br, button)
import Html.Attributes exposing (placeholder, maxlength, size, value)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt, toList, fromList)
import Char exposing (toLower, isLower)
import List exposing (head, length, reverse, take, drop, append, repeat, filter)
import Maybe exposing (map, withDefault)
import Set exposing (Set, toList, fromList, empty)
import Maybe.Extra exposing (combine)
import Debug exposing (crash, log)

main : Program Never
main = Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


---------- Types ----------

type alias Model =
    { length : Maybe Int
    , secret : List (Maybe Char)
    , guesses : Set Char
    , wordlist : List String
    }

type Msg
    = ChangeLength String
    | ChangeLetter Int String
    | AddGuess String
    | ResetGame


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
                    , value <| withDefault "" (Maybe.map toString model.length)
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
                , model.guesses
                    |> Set.toList
                    |> String.fromList
                    |> value
                ]
                []
            ]
        , div
            []
            [ button
                [ onClick ResetGame ]
                [ text "Reset" ]
            ]
        , div
            []
            [ br [] []
            , text <| toString model
            ]
        ]


viewLetters : Model -> List (Html Msg)
viewLetters model =
    let
        letters = case model.length of
                      Just n ->
                          [0..(n-1)]
                      Nothing ->
                          []
        createLetter i =
            input
                [ maxlength 1
                , size 1 
                , onInput <| ChangeLetter i
                , value <| withDefault "" (model.secret
                                            |> drop i 
                                            |> take 1
                                            |> combine
                                            |> Maybe.map String.fromList
                                          )
                ]
                []
    in
        List.map createLetter letters

    

---------- State ----------

init : (Model, Cmd a)
init =
    { length = Nothing
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
    } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLength s ->
            case (toInt s) of
                Ok i ->
                    { model
                        | length = Just i
                        , secret = repeat i Nothing
                    } ! []
                Err _ ->
                    model ! []
        ChangeLetter i s ->
            let
                updated = updateGuess
                              i
                              s
                              model.secret
                m = { model | secret = updated }
            in
                update (AddGuess s) m
        AddGuess s ->
            { model |
                guesses = s
                           |> String.toList
                           |> filter isAlpha
                           |> Set.fromList
                           |> Set.union model.guesses
            } ! []
        ResetGame ->
            init

isAlpha : Char -> Bool
isAlpha = isLower << toLower

updateGuess : Int -> String -> List (Maybe Char) -> List (Maybe Char)
updateGuess i chars secret =
    let
        char = chars
                |> String.toList
                |> filter isAlpha
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
                    append prefix (Just c :: suffix)
