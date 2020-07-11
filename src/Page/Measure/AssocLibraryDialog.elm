module Page.Measure.AssocLibraryDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.List as List
import Material.List.Item as ListItem
import Material.TextField as TextField
import Maybe.Extra as MaybeExtra
import Process
import Task
import Url.Builder as UrlBuilder



-- MODEL


type Model
    = Closed { base : String }
    | Open
        { base : String
        , libraries : List Library
        , search : Maybe String
        }


init base =
    Closed { base = base }



-- UPDATE


type Msg
    = ClickedClose
    | EnteredSearch String
    | MaybePerformSearch String
    | CompletedLoad (Result FhirHttp.Error Bundle)


doOpen : Model -> ( Model, Cmd Msg )
doOpen model =
    case model of
        Open _ ->
            ( model, Cmd.none )

        Closed { base } ->
            ( Open { base = base, libraries = [], search = Nothing }
            , searchLibraries base ""
            )


doClose : Model -> Model
doClose model =
    case model of
        Open { base } ->
            Closed { base = base }

        Closed { base } ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedClose, _ ) ->
            ( doClose model, Cmd.none )

        ( EnteredSearch search, Open data ) ->
            ( Open { data | search = Just search }
            , Process.sleep 200
                |> Task.map (\_ -> search)
                |> Task.perform MaybePerformSearch
            )

        ( MaybePerformSearch search, Open data ) ->
            if Just search == data.search then
                ( model, searchLibraries data.base search )

            else
                ( model, Cmd.none )

        ( CompletedLoad (Ok bundle), Open data ) ->
            if bundle.type_ == "searchset" then
                ( Open { data | libraries = decodeLibraries bundle }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


searchLibraries base query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType CompletedLoad base "Library" params


decodeLibraries : Bundle -> List Library
decodeLibraries { entry } =
    List.filterMap
        (.resource >> decodeValue Library.decoder >> Result.toMaybe)
        entry



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSelect : Library -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSelect } model =
    let
        ( open, search, libraries ) =
            case model of
                Open data ->
                    ( True, data.search, data.libraries )

                Closed _ ->
                    ( False, Nothing, [] )
    in
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen open
            |> Dialog.setOnClose (onMsg ClickedClose)
            |> Dialog.setAttributes [ class "measure-assoc-library-dialog" ]
        )
        { title = Just "Load Library"
        , content =
            [ TextField.filled
                (TextField.config
                    |> TextField.setPlaceholder (Just "Search")
                    |> TextField.setValue search
                    |> TextField.setOnInput (EnteredSearch >> onMsg)
                    |> TextField.setFullwidth True
                )
            , libraryList onSelect libraries
            ]
        , actions = []
        }


libraryList onSelect libraries =
    case libraries of
        library :: moreLibraries ->
            List.list (List.config |> List.setTwoLine True)
                (libraryListItem onSelect library)
                (List.map (libraryListItem onSelect) moreLibraries)

        _ ->
            text "no libraries available"


libraryListItem onSelect library =
    let
        disabled =
            MaybeExtra.isNothing library.url
    in
    ListItem.listItem
        (ListItem.config
            |> ListItem.setOnClick (onSelect library)
            |> ListItem.setDisabled disabled
        )
        [ ListItem.text []
            { primary = [ titleText library ]
            , secondary = [ urlText library ]
            }
        , meta disabled
        ]


titleText { title, name } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault "<unknown>"
        |> text


urlText { url } =
    url
        |> Maybe.withDefault "Libraries without URL's can't be referenced."
        |> text


meta disabled =
    if disabled then
        ListItem.meta [] [ Icon.icon [] "warning" ]

    else
        text ""
