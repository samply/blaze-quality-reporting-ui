module Json.Decode.Zipper exposing (zipper)

import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import List.Zipper as Zipper exposing (Zipper)


zipper : Decoder a -> Decoder (Zipper a)
zipper decoder =
    succeed Zipper.from
        |> optional "before" (list decoder) []
        |> required "current" decoder
        |> optional "after" (list decoder) []
