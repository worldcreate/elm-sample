import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key, pushUrl, load)
import Url exposing (Url)
import Html exposing (Html, button, div, text, h1, input, ul, li, a)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value, href)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, fragment)

import Ports exposing (saveTodos, fetchTodos, sendTodos)



main = 
    Browser.application {
        init = init,
        update = update,
        view = view,
        subscriptions = subscriptions,
        onUrlRequest = UrlRequest,
        onUrlChange = UrlChange
    }

-- MODEL

type alias Model =
    {
        key: Key,
        page: Page
    }

type Page =
    Home HomeModel
    | DetailPage DetailModel

type alias HomeModel = {
    input: String,
    todos: List String
    }

type alias DetailModel = {
    todoName: String
    }

init : () -> Url -> Key -> (Model, Cmd Msg)
init flags url key = ({
    key = Debug.log "init key" key,
    page = Home {input = "",
    todos = []
    }}, Cmd.none)

-- UPDATE

type Msg = 
    Input String
    | Save
    | Delete Int
    | UrlRequest UrlRequest
    | UrlChange Url
    | GetTodos (List String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        _ = Debug.log "msg" msg
    in
    case msg of
        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, pushUrl model.key (Url.toString url))
                        
                Browser.External href ->
                    (model, load href)
        UrlChange url ->
            let
                _ = Debug.log "urlChange url" url
            in
                let
                    nextModel = onUrlChange url model
                in
                case nextModel.page of
                    Home _ ->
                        (nextModel, fetchTodos ())
                    _ ->
                        (nextModel, Cmd.none)
        GetTodos todos ->
            case model.page of
                Home homeModel ->
                    ({model | page = Home {homeModel | todos = todos }}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        _ ->
            case model.page of
                Home homeModel ->
                    case msg of
                        Input newInput ->
                            ({model | page = Home {homeModel | input = newInput }}, Cmd.none)
                        Save ->
                            let
                                newTodos = homeModel.input :: homeModel.todos
                            in
                            
                            ({model | page = Home {homeModel | todos = newTodos, input = ""}}, saveTodos newTodos)
                        Delete index ->
                            let
                                t = homeModel.todos
                            in
                                ({model | page = Home {homeModel | todos = List.take index t ++ List.drop (index + 1) t}}, Cmd.none)
                        _ ->
                            (model, Cmd.none)
                DetailPage detail ->
                    ({model | page = DetailPage detail}, Cmd.none)

-- VIEW

view: Model -> Document Msg
view model = ({
    title = "TODO!",
    body = 
        case model.page of
            Home homeModel ->
                [div [] [
                    h1 [] [text "TODO!!"],
                    div [] [
                        ul [] (
                            List.indexedMap (\i todoName -> 
                                div [] [
                                    li [] [ a [ href ("#" ++ todoName) ] [ text todoName ] ],
                                    button [ onClick (Delete i) ] [text "delete"]
                                    ]) homeModel.todos)
                    ],
                    div [] [
                        input [ onInput Input, value homeModel.input ] [],
                        div [] [button [ onClick Save ] [text "save"]]
                    ]
                ]]
            DetailPage detailModel ->
                [div [][text detailModel.todoName]]
    })

subscriptions: Model -> Sub Msg
subscriptions model =
    sendTodos GetTodos

onUrlChange: Url -> Model -> Model
onUrlChange url model =
    let
        maybeRoute = Parser.parse routeParser (Debug.log "onUrlChange url" url)
    in
    case maybeRoute of
        Nothing ->
            Debug.log "UrlChange Nothing" model
        Just (Top (Nothing)) ->
            Debug.log "UrlChange Just Nothing" {model | page = Home {
                input = "",
                todos = []
            }}
        Just (Top (Just todoName)) ->
            Debug.log "UrlChange Just Detail" ({model | page = DetailPage {todoName = todoName}})

type Route
    = Top (Maybe String)

routeParser: Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Top (fragment identity) ]