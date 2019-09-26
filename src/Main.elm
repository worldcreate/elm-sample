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
    | Delete Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newInput ->
            ({model | input = newInput })
        Save ->
            ({model | todos = model.input :: model.todos, input = ""})
        Delete index ->
            let
                t = model.todos
            in
                ({model | todos = List.take index t ++ List.drop (index + 1) t})

-- VIEW

view: Model -> Html Msg
view model =
    div [] [
        h1 [] [text "TODO!!"],
        div [] [
            ul [] (List.indexedMap (\i a -> div [] [li [] [text a], button [ onClick (Delete i) ] [text "delete"]]) model.todos)
        ],
        div [] [
            input [ onInput Input, value model.input ] [],
            div [] [button [ onClick Save ] [text "save"]]
        ]
    
    ]