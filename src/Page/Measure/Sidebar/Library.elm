module Page.Measure.Sidebar.Library exposing (view)

import Component.Sidebar
    exposing
        ( SidebarEntry
        , sidebarEditButton
        , sidebarEditButtonConfig
        , sidebarEntry
        , sidebarEntryConfig
        , sidebarEntryContent
        , sidebarEntryTitle
        )
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (Html, a, text)
import Route exposing (href)



-- VIEW


type alias Config msg =
    { onEdit : msg }


view : Config msg -> List Canonical -> SidebarEntry msg
view { onEdit } library =
    sidebarEntry sidebarEntryConfig
        [ sidebarEntryTitle []
            [ text "Library"
            , sidebarEditButton
                { sidebarEditButtonConfig | onClick = Just onEdit }
            ]
        , sidebarEntryContent []
            [ libraryLink library ]
        ]


libraryLink : List Canonical -> Html msg
libraryLink uris =
    uris
        |> List.head
        |> Maybe.map (\uri -> a [ href (Route.LibraryByUrl uri) ] [ text uri ])
        |> Maybe.withDefault (text "No associated library")
