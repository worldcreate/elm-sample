import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Html exposing (Html, button, div, text, h1, input, ul, li)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value)

main = 
    Browser.application {
        init = init,
        update = update,
        view = view,
        subscriptions = subscriptions,
        onUrlRequest = onUrlRequest,
        onUrlChange = onUrlChange
    }

-- MODEL

type alias Model = {
    input: String,
    todos: List String
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init flags url key = ({
    input = "",
    todos = []
    }, Cmd.none)

-- UPDATE

type Msg = 
    Input String
    | Save
    | Delete Int
    | UrlRequest UrlRequest
    | UrlChange Url

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
        UrlRequest urlRequest ->
            (model, Cmd.none)
        UrlChange url ->
            (model, Cmd.none)

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

onUrlRequest: UrlRequest -> Msg
onUrlRequest urlRequest =
    UrlRequest urlRequest

onUrlChange: Url -> Msg
onUrlChange url =
    UrlChange url