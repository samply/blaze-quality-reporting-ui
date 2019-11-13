module Fhir.Encode exposing (object, optionalListPair, optionalPair, pair)

import Json.Encode as Encode exposing (Value)


{-| Like Json.Encode.object but pairs can be optional.
-}
object : List (Maybe ( String, Value )) -> Value
object pairs =
    Encode.object (List.filterMap identity pairs)


pair : String -> (a -> Value) -> a -> Maybe ( String, Value )
pair key f entry =
    Just ( key, f entry )


optionalPair : String -> (a -> Value) -> Maybe a -> Maybe ( String, Value )
optionalPair key f entry =
    Maybe.map (\e -> ( key, f e )) entry


{-| Encodes a list of entries into optional pairs.
-}
optionalListPair : String -> (a -> Value) -> List a -> Maybe ( String, Value )
optionalListPair key f entries =
    if List.isEmpty entries then
        Nothing

    else
        Just ( key, Encode.list f entries )
