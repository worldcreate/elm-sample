port module Ports exposing (saveTodos, fetchTodos, sendTodos)

port saveTodos: List String -> Cmd msg
port fetchTodos: () -> Cmd msg
port sendTodos: (List String -> msg) -> Sub msg