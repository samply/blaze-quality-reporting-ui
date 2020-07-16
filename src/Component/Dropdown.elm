module Component.Dropdown exposing
    ( Model
    , Msg
    , close
    , config
    , init
    , update
    , view
    )

import Component.List as List
import Component.List.Item as ListItem exposing (ListItem)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events



-- MODEL


type alias Model =
    { open : Bool
    }


init : Model
init =
    { open = False }



-- UPDATE


type Msg
    = Clicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked ->
            { model | open = not model.open }


close : Model -> Model
close model =
    { model | open = False }



-- VIEW


type alias Config item msg =
    { onMsg : Msg -> msg
    , onSelect : item -> msg
    }


{-| Default configuration of a dropdown
-}
config : (Msg -> msg) -> (item -> msg) -> Config item msg
config onMsg onSelect =
    { onMsg = onMsg, onSelect = onSelect }


view : Config item msg -> String -> item -> List ( item, String ) -> Model -> Html msg
view { onMsg, onSelect } label selectedItem items { open } =
    Html.div [ class "relative" ]
        [ Html.button
            [ class "text-sm font-medium"
            , class "text-gray-800 hover:text-gray-900"
            , class "focus:outline-none"
            , Html.Events.onClick (onMsg Clicked)
            ]
            [ Html.text label
            , Html.span
                [ class "inline-block w-0 h-0 align-middle ml-1"
                , class "border-4 border-b-0 border-current"
                , style "border-bottom-color" "transparent"
                , style "border-left-color" "transparent"
                , style "border-right-color" "transparent"
                ]
                []
            ]
        , List.list
            (List.config
                |> List.setDense True
                |> List.setAttributes
                    [ class "absolute inset-auto right-0"
                    , if open then
                        class ""

                      else
                        class "hidden"
                    ]
            )
            (items
                |> List.map
                    (\x ->
                        listItemView onSelect selectedItem x
                    )
            )
        ]


listItemView : (item -> msg) -> item -> ( item, String ) -> ListItem msg
listItemView onSelect selectedItem ( item, label ) =
    ListItem.listItem
        (ListItem.config
            |> ListItem.setOnClick (onSelect item)
            |> ListItem.setAttributes [ class "text-sm" ]
        )
        label
        [ Html.div [ class "flex" ]
            [ Html.span [ class "inline-block w-4" ]
                [ Html.text <|
                    if selectedItem == item then
                        "✓"

                    else
                        ""
                ]
            , Html.span [ class "inline-block flex-grow whitespace-no-wrap" ]
                [ Html.text label ]
            ]
        ]
