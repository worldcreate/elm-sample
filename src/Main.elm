import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, input, ul, li)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value)

main = 
    Browser.document {
        init = init,
        update = update,
        view = view,
        subscriptions = subscriptions
    }

-- MODEL

type alias Model = {
    input: String,
    todos: List String
    }

init : () -> (Model, Cmd Msg)
init flags = ({
    input = "",
    todos = []
    }, Cmd.none)

-- UPDATE

type Msg = 
    Input String
    | Save
    | Delete Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input newInput ->
            ({model | input = newInput }, Cmd.none)
        Save ->
            ({model | todos = model.input :: model.todos, input = ""}, Cmd.none)
        Delete index ->
            let
                t = model.todos
            in
                ({model | todos = List.take index t ++ List.drop (index + 1) t}, Cmd.none)

-- VIEW

view: Model -> Document Msg
view model = ({
    title = "TODO!",
    body = [div [] [
        h1 [] [text "TODO!!"],
        div [] [
            ul [] (List.indexedMap (\i a -> div [] [li [] [text a], button [ onClick (Delete i) ] [text "delete"]]) model.todos)
        ],
        div [] [
            input [ onInput Input, value model.input ] [],
            div [] [button [ onClick Save ] [text "save"]]
        ]
    ]]
    })

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none