module Component.Dialog exposing
    ( config
    , dialog
    , setAttributes
    , setOnClose
    , setOpen
    )

import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Config msg =
    { open : Bool
    , onClose : Maybe msg
    , additionalAttributes : List (Html.Attribute msg)
    }


{-| Default configuration of a dialog
-}
config : Config msg
config =
    { open = False
    , onClose = Nothing
    , additionalAttributes = []
    }


{-| Specify whether a dialog is open
-}
setOpen : Bool -> Config msg -> Config msg
setOpen open config_ =
    { config_ | open = open }


{-| Specify a message when the user closes the dialog
-}
setOnClose : msg -> Config msg -> Config msg
setOnClose onClose config_ =
    { config_ | onClose = Just onClose }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


{-| Dialog content
-}
type alias Content msg =
    { title : Maybe String
    , content : List (Html msg)
    , actions : List (Html msg)
    }


{-| Dialog view function
-}
dialog : Config msg -> Content msg -> Html msg
dialog ({ additionalAttributes } as config_) content =
    Html.div
        ([ class "fixed inset-0 h-screen"
         , class "z-50 bg-black bg-opacity-32"
         ]
            ++ List.filterMap identity
                [ openClass config_

                --, closeHandler config_
                ]
        )
        [ viewDialog additionalAttributes content
        ]


openClass { open } =
    if open then
        Nothing

    else
        Just (class "hidden")


viewDialog : List (Html.Attribute msg) -> Content msg -> Html msg
viewDialog additionalAttributes { title, content, actions } =
    Html.div []
        [ Html.div
            ([ class "fixed top-1/4 left-1/4"
             , class "w-1/2 max-h-1/2"
             , class "bg-white rounded-md p-6 shadow-2xl"
             ]
                ++ additionalAttributes
            )
            [ Maybe.map viewTitle title |> Maybe.withDefault (Html.text "")
            , viewContent content
            , viewActions actions
            ]
        ]


viewTitle : String -> Html msg
viewTitle title =
    Html.div [ class "text-xl font-medium mb-4" ]
        [ Html.text title ]


viewContent : List (Html msg) -> Html msg
viewContent content =
    Html.div [ class "mb-6" ] content


viewActions : List (Html msg) -> Html msg
viewActions actions =
    Html.div [ class "text-right space-x-2" ] actions
