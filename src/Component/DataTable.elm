module Component.DataTable exposing
    ( Cell
    , Config
    , Row
    , cell
    , config
    , dataTable
    , numericCell
    , row
    , setAttributes
    , setLabel
    )

import Html exposing (Html)
import Html.Attributes exposing (class, style)


type Config msg
    = Config
        { label : Maybe String
        , additionalAttributes : List (Html.Attribute msg)
        }


{-| Default configuration of a data table
-}
config : Config msg
config =
    Config
        { label = Nothing
        , additionalAttributes = []
        }


{-| Specify the data table's HTML5 aria-label attribute
-}
setLabel : Maybe String -> Config msg -> Config msg
setLabel label (Config config_) =
    Config { config_ | label = label }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Data table view function
-}
dataTable :
    Config msg
    ->
        { thead : List (Row msg)
        , tbody : List (Row msg)
        }
    -> Html msg
dataTable ((Config { additionalAttributes }) as config_) { thead, tbody } =
    Html.div
        ([ class "border border-gray-400 rounded-md overflow-hidden" ]
            ++ additionalAttributes
        )
        [ Html.table
            [ class "w-full" ]
            [ Html.thead [ class "" ] (List.map headerRow thead)
            , Html.tbody [ class "bg-white" ] (List.map bodyRow tbody)
            ]
        ]


{-| Row type
-}
type Row msg
    = Row { attributes : List (Html.Attribute msg), nodes : List (Cell msg) }


{-| Row view function
-}
row : List (Html.Attribute msg) -> List (Cell msg) -> Row msg
row attributes nodes =
    Row { attributes = attributes, nodes = nodes }


headerRow : Row msg -> Html msg
headerRow (Row { attributes, nodes }) =
    Html.tr (class "" :: attributes) (List.map headerCell nodes)


bodyRow : Row msg -> Html msg
bodyRow (Row { attributes, nodes }) =
    Html.tr ([ class "" ] ++ attributes) <|
        List.map bodyCell nodes


{-| Cell type
-}
type Cell msg
    = Cell
        { numeric : Bool
        , attributes : List (Html.Attribute msg)
        , nodes : List (Html msg)
        }


{-| Data table cell
-}
cell : List (Html.Attribute msg) -> List (Html msg) -> Cell msg
cell attributes nodes =
    Cell { numeric = False, attributes = attributes, nodes = nodes }


{-| Numeric data table cell (right-aligned contents)
-}
numericCell : List (Html.Attribute msg) -> List (Html msg) -> Cell msg
numericCell attributes nodes =
    Cell { numeric = True, attributes = attributes, nodes = nodes }


headerCell : Cell msg -> Html msg
headerCell cell_ =
    case cell_ of
        Cell { numeric, attributes, nodes } ->
            Html.th
                ([ class "bg-blue-100 font-medium px-4 py-2 border border-t-0 border-l-0 last:border-r-0 border-gray-400"
                 , Html.Attributes.attribute "scope" "col"
                 ]
                    ++ attributes
                )
                nodes


bodyCell : Cell msg -> Html msg
bodyCell cell_ =
    case cell_ of
        Cell { numeric, attributes, nodes } ->
            Html.td
                ([ class "px-4 py-2 border border-b-0 border-l-0 last:border-r-0 border-gray-400"
                 , if numeric then
                    class "text-right"

                   else
                    class ""
                 ]
                    ++ attributes
                )
                nodes
