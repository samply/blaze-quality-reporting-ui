module Fhir.CodeableConcept exposing
    ( CodeableConcept
    , decoder
    , encode
    , getCodeOf
    , ofOneCoding
    , setCodeOf
    )

import Fhir.Coding as Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias CodeableConcept =
    { coding : List Coding
    , text : Maybe String
    }


ofOneCoding : Coding -> CodeableConcept
ofOneCoding coding =
    { coding = [ coding ]
    , text = Nothing
    }


{-| Returns the code of the coding with the given system.
-}
getCodeOf : String -> CodeableConcept -> Maybe String
getCodeOf system =
    getCodingOf system >> Maybe.andThen .code


getCodingOf : String -> CodeableConcept -> Maybe Coding
getCodingOf system =
    .coding >> List.filter (.system >> (==) (Just system)) >> List.head


{-| Sets the code of the coding with the given system.

Creates a new coding if there isn't one with the given system.

-}
setCodeOf : String -> String -> CodeableConcept -> CodeableConcept
setCodeOf system code codeableConcept =
    case getCodingOf system codeableConcept of
        Just _ ->
            let
                f coding =
                    { coding | code = Just code }
            in
            { codeableConcept | coding = List.map f codeableConcept.coding }

        Nothing ->
            let
                coding =
                    { system = Just system
                    , version = Nothing
                    , code = Just code
                    }
            in
            { codeableConcept | coding = coding :: codeableConcept.coding }


encode : CodeableConcept -> Value
encode { coding, text } =
    object
        [ optionalListPair "coding" Coding.encode coding
        , optionalPair "text" Encode.string text
        ]


decoder : Decoder CodeableConcept
decoder =
    succeed CodeableConcept
        |> optional "coding" (list Coding.decoder) []
        |> optional "text" (maybe string) Nothing
