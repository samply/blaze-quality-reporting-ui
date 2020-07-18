module Fhir.PrimitiveTypes.Instant exposing (Instant, decoder, encode)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Parser
import Time


type alias Instant =
    Time.Posix


encode : Instant -> Value
encode =
    Iso8601.fromTime >> Encode.string


decoder : Decoder Instant
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case Iso8601.toTime s of
                    Ok time ->
                        Decode.succeed time

                    Err deadEnds ->
                        Decode.fail (Parser.deadEndsToString deadEnds)
            )
