import Browser
import Html exposing (Html, button, div, text, h1, input)
import Html.Events exposing (onInput)

main = 
    Browser.sandbox {
        init = init,
        update = update,
        view = view
    }

-- MODEL

type alias Model = {
    input: String
    }

init : Model
init = {
    input = ""
    }

-- UPDATE

type Msg = Input String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newInput ->
            ({model | input = newInput })

-- VIEW

view: Model -> Html Msg
view model =
    div [] [h1 [] [text "TODO!!"], div [] [input [ onInput Input ] []], div [] [text model.input] ]