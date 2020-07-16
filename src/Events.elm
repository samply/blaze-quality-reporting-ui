module Events exposing
    ( Key(..)
    , keyDecoder
    , onEnter
    , onEnterEsc
    , onEnterEsc2
    , onEsc
    , onSlash
    )

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode exposing (Decoder)


onEnterEsc : msg -> msg -> Attribute msg
onEnterEsc enterMsg escMsg =
    let
        withCode code =
            case code of
                13 ->
                    Decode.succeed enterMsg

                27 ->
                    Decode.succeed escMsg

                _ ->
                    Decode.fail "???"
    in
    on "keyup" (Decode.andThen withCode keyCode)


onEnterEsc2 : Maybe msg -> Maybe msg -> Attribute msg
onEnterEsc2 enterMsg escMsg =
    let
        withCode code =
            case code of
                13 ->
                    enterMsg
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "???")

                27 ->
                    escMsg
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "???")

                _ ->
                    Decode.fail "???"
    in
    on "keyup" (Decode.andThen withCode keyCode)


onEnter : msg -> Attribute msg
onEnter msg =
    on "keyup" (Decode.andThen (isCode 13 msg) keyCode)


onEsc : msg -> Attribute msg
onEsc msg =
    on "keyup" (Decode.andThen (isCode 27 msg) keyCode)


onSlash : msg -> Attribute msg
onSlash msg =
    on "keyup" (Decode.andThen (isCode 191 msg) keyCode)


isCode expected msg code =
    if code == expected then
        Decode.succeed msg

    else
        Decode.fail "???"


type Key
    = Slash
    | Esc


keyDecoder : (Key -> msg) -> Decoder msg
keyDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "/" ->
                        Decode.succeed (msg Slash)

                    "Escape" ->
                        Decode.succeed (msg Esc)

                    _ ->
                        Decode.fail "?"
            )
