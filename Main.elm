import Html.App exposing (beginnerProgram)
import Html exposing (Html, div, text )

main = Html.App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


---------- Types ----------

type alias Model = String


---------- View ----------

view : Model -> Html a
view model =
    div
        []
        [text model]


---------- State ----------

model : Model
model = "Foobar"

update : a -> Model -> Model
update msg model = model
