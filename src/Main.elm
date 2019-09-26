import Browser
import Html exposing (Html, button, div, text, h1, input, ul, li)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value)

main = 
    Browser.sandbox {
        init = init,
        update = update,
        view = view
    }

-- MODEL

type alias Model = {
    input: String,
    todos: List String
    }

init : Model
init = {
    input = "",
    todos = []
    }

-- UPDATE

type Msg = 
    Input String
    | Save

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newInput ->
            ({model | input = newInput })
        Save ->
            ({model | todos = List.concat [model.todos, [model.input]], input = ""})

-- VIEW

view: Model -> Html Msg
view model =
    div [] [
        h1 [] [text "TODO!!"],
        div [] [
            ul [] (List.map (\a -> li [] [text a]) model.todos)
        ],
        div [] [
            input [ onInput Input, value model.input ] [],
            div [] [button [ onClick Save ] [text "save"]]
        ]
    
    ]