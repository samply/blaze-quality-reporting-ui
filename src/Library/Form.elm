module Library.Form exposing (Model, Msg, init, update, view)

import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Html exposing (Html, div, p)
import Html.Attributes exposing (style)
import Http
import Material.Button exposing (buttonConfig, unelevatedButton)
import Material.TextArea exposing (textArea, textAreaConfig)
import Material.TextField exposing (textField, textFieldConfig)



-- MODEL


type alias Model =
    { base : String
    , library : Library
    }


init : String -> Library -> Model
init base library =
    { base = base
    , library = library
    }



-- UPDATE


type Msg
    = EnteredTitle String
    | EnteredCql String
    | ClickedSave
    | CompletedSave (Result Http.Error Library)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredTitle title ->
            let
                library =
                    model.library
            in
            ( { model | library = { library | title = Just title } }
            , Cmd.none
            )

        EnteredCql cql ->
            let
                library =
                    model.library
            in
            ( { model | library = { library | content = [ attachment cql ] } }
            , Cmd.none
            )

        ClickedSave ->
            ( model, saveLibrary model )

        CompletedSave (Ok library) ->
            ( { model | library = library }, Cmd.none )

        CompletedSave (Err _) ->
            ( model, Cmd.none )


attachment cql =
    { contentType = Just "text/cql"
    , data = Just cql
    }


saveLibrary { base, library } =
    let
        json =
            Library.encode library
    in
    case library.id of
        Just id ->
            FhirHttp.update CompletedSave base "Library" id Library.decoder json

        Nothing ->
            FhirHttp.create CompletedSave base "Library" Library.decoder json



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onClose : msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onClose } { library } =
    div []
        [ p [] [ titleField library.title |> Html.map onMsg ]
        , p []
            [ cqlTextarea (library.content |> List.head |> Maybe.andThen .data)
                |> Html.map onMsg
            ]
        , p [] [ saveButton library |> Html.map onMsg, closeButton onClose ]
        ]


titleField title =
    textField
        { textFieldConfig
            | label = Just "Title"
            , value = title
            , onInput = Just EnteredTitle
        }


cqlTextarea cql =
    textArea
        { textAreaConfig
            | label = Just "CQL"
            , value = cql
            , onInput = Just EnteredCql
            , rows = Just 8
            , cols = Just 80
        }


saveButton { id } =
    unelevatedButton
        { buttonConfig | onClick = Just ClickedSave }
        (case id of
            Just _ ->
                "Update"

            Nothing ->
                "Save"
        )


closeButton onClose =
    unelevatedButton
        { buttonConfig
            | onClick = Just onClose
            , additionalAttributes = [ style "margin-left" "1em" ]
        }
        "Close"
