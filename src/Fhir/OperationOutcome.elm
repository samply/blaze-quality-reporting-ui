module Fhir.OperationOutcome exposing
    ( Issue
    , OperationOutcome
    , Severity(..)
    , Type(..)
    , decoder
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Json.Decode as Decode exposing (Decoder, fail, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias OperationOutcome =
    { issue : List Issue }


type alias Issue =
    { severity : Severity
    , code : Type
    , details : Maybe CodeableConcept
    , diagnostics : Maybe String
    , expression : List String
    }


type Severity
    = Fatal
    | Error
    | Warning
    | Information


type Type
    = Invalid
    | Structure
    | Required
    | Value
    | Invariant
    | Security
    | Login
    | Unknown
    | Expired
    | Forbidden
    | Suppressed
    | Processing
    | NotSupported
    | Duplicate
    | MultipleMatches
    | NotFound
    | Deleted
    | TooLong
    | CodeInvalid
    | Extension
    | TooCostly
    | BusinessRule
    | Conflict
    | Transient
    | LockError
    | NoStore
    | Exception
    | Timeout
    | Incomplete
    | Throttled
    | Informational


decoder : Decoder OperationOutcome
decoder =
    succeed OperationOutcome
        |> optional "issue" (list issueDecoder) []


issueDecoder : Decoder Issue
issueDecoder =
    succeed Issue
        |> required "severity" severityDecoder
        |> required "code" typeDecoder
        |> optional "details" (maybe CodeableConcept.decoder) Nothing
        |> optional "diagnostics" (maybe string) Nothing
        |> optional "expression" (list string) []


severityDecoder : Decoder Severity
severityDecoder =
    Decode.andThen
        (\s ->
            case s of
                "fatal" ->
                    succeed Fatal

                "error" ->
                    succeed Error

                "warning" ->
                    succeed Warning

                "information" ->
                    succeed Information

                _ ->
                    fail ("Invalid IssueSeverity `" ++ s ++ "`.")
        )
        string


typeDecoder : Decoder Type
typeDecoder =
    Decode.andThen
        (\s ->
            case s of
                "invalid" ->
                    succeed Invalid

                "structure" ->
                    succeed Structure

                "required" ->
                    succeed Required

                "value" ->
                    succeed Value

                "invariant" ->
                    succeed Invariant

                "security" ->
                    succeed Security

                "login" ->
                    succeed Login

                "unknown" ->
                    succeed Unknown

                "expired" ->
                    succeed Expired

                "forbidden" ->
                    succeed Forbidden

                "suppressed" ->
                    succeed Suppressed

                "processing" ->
                    succeed Processing

                "not-supported" ->
                    succeed NotSupported

                "duplicate" ->
                    succeed Duplicate

                "multiple-matches" ->
                    succeed MultipleMatches

                "not-found" ->
                    succeed NotFound

                "deleted" ->
                    succeed Deleted

                "too-long" ->
                    succeed TooLong

                "code-invalid" ->
                    succeed CodeInvalid

                "extension" ->
                    succeed Extension

                "too-costly" ->
                    succeed TooCostly

                "business-rule" ->
                    succeed BusinessRule

                "conflict" ->
                    succeed Conflict

                "transient" ->
                    succeed Transient

                "lock-error" ->
                    succeed LockError

                "no-store" ->
                    succeed NoStore

                "exception" ->
                    succeed Exception

                "timeout" ->
                    succeed Timeout

                "incomplete" ->
                    succeed Incomplete

                "throttled" ->
                    succeed Throttled

                "informational" ->
                    succeed Informational

                _ ->
                    fail ("Invalid IssueType `" ++ s ++ "`.")
        )
        string
