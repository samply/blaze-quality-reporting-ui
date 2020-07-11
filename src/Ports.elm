port module Ports exposing (clipboardFailure, clipboardSuccess, storeSession, writeToClipboard)

import Json.Encode exposing (Value)


port storeSession : String -> Cmd msg


port writeToClipboard : String -> Cmd msg


port clipboardSuccess : (Value -> msg) -> Sub msg


port clipboardFailure : (Value -> msg) -> Sub msg
