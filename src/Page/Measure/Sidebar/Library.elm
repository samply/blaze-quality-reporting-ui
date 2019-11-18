module Page.Measure.Sidebar.Library exposing (view)

import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Material.Button exposing (buttonConfig, textButton)
import Route exposing (href)


type alias Config msg =
    { onEdit : msg }


view : Config msg -> List Canonical -> Html msg
view { onEdit } library =
    div [ class "right-sidebar__entry" ]
        [ div [ class "right-sidebar__title mdc-typography--subtitle1" ]
            [ text "Library"
            , textButton
                { buttonConfig | dense = True, onClick = Just onEdit }
                "edit"
            ]
        , div [ class "right-sidebar__content mdc-typography--body2" ]
            [ libraryLink library ]
        ]


libraryLink : List Canonical -> Html msg
libraryLink uris =
    uris
        |> List.head
        |> Maybe.map (\uri -> a [ href (Route.LibraryByUrl uri) ] [ text uri ])
        |> Maybe.withDefault (text "No associated library")
