module Page.Library.List.Query exposing (Query)

import Fhir.ValueSet.PublicationStatus exposing (PublicationStatus)
import Page.Library.List.Sort exposing (Sort)


type alias Query =
    { status : PublicationStatus
    , title : Maybe String
    , sort : Sort
    }
