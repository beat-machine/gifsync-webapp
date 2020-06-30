port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Base64
import Browser
import Bytes exposing (Bytes)
import Common.Content
import FeatherIcons
import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


type alias Flags =
    { baseUrl : String
    , version : String
    }


type Status
    = Idle
    | Failed String
    | InProgress
    | Succeeded


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { gif : Maybe File.File
    , audio : Maybe File.File
    , apiUrl : String
    , version : String
    , status : Status
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gif = Nothing
      , audio = Nothing
      , apiUrl = flags.baseUrl
      , version = flags.version
      , status = Idle
      }
    , Cmd.none
    )


type Msg
    = SetGif File.File
    | SetAudio File.File
    | Submit
    | GotVideo (Result String Bytes)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGif gifFile ->
            ( { model | gif = Just gifFile }, Cmd.none )

        SetAudio audioFile ->
            ( { model | audio = Just audioFile }, Cmd.none )

        Submit ->
            case ( model.gif, model.audio ) of
                ( Just gifFile, Just audioFile ) ->
                    ( { model | status = InProgress }
                    , Cmd.batch
                        [ clearVideo ()
                        , Api.submitFiles model.apiUrl { gif = gifFile, audio = audioFile } GotVideo
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotVideo (Err error) ->
            ( { model | status = Failed error }, Cmd.none )

        GotVideo (Ok videoBytes) ->
            case Base64.fromBytes videoBytes of
                Just d ->
                    ( { model | status = Succeeded }, setVideo d )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewUpload : Model -> Html Msg
viewUpload model =
    div [ class "row" ]
        [ section [ class "frame six columns upload-panel" ]
            [ h3 [] [ text "Gif" ]
            , p [] [ text "Upload an animated GIF. If it's excessively large or not animated, it'll probably fail." ]
            , input
                [ type_ "file"
                , multiple False
                , accept "image/gif, .gif"
                , on "change" (D.map SetGif filesDecoder)
                ]
                []
            ]
        , section [ class "frame six columns upload-panel" ]
            [ h3 [] [ text "Audio" ]
            , p [] [ text "Upload an MP3 up to 2:30 in length. Longer songs will break!" ]
            , input
                [ type_ "file"
                , multiple False
                , accept "audio/mpeg, .mp3"
                , on "change" (D.map SetAudio filesDecoder)
                ]
                []
            ]
        ]


loader : Html Msg
loader =
    div []
        [ p [ class "status" ] [ text "Working on it..." ]
        , div [ class "loader" ]
            [ div [ id "r1" ] []
            , div [ id "r2" ] []
            , div [ id "r3" ] []
            , div [ id "r4" ] []
            ]
        ]


viewResult : Model -> Html Msg
viewResult model =
    section [ class "frame" ]
        [ h3 [] [ text "Result" ]
        , p [] [ text "Press the button below to render the result!" ]
        , div [ class "render-button-container" ]
            [ button [ class "button-primary render-button", disabled (model.audio == Nothing || model.gif == Nothing || model.status == InProgress), onClick Submit ] [ text "Submit!" ]
            ]
        , case model.status of
            InProgress ->
                loader

            Failed errorMsg ->
                p [ class "status", class "error" ] [ text errorMsg ]

            _ ->
                text ""
        , video [ classList [ ( "hidden", model.status /= Succeeded ) ], id "player", controls True, autoplay False ] []
        , p [ classList [ ( "hidden", model.status /= Succeeded ) ], class "audio-hint" ]
            [ text "Right-click on the player above or "
            , a
                [ id "download"
                , download ""
                ]
                [ text "use this link" ]
            , text " to download the result."
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container"
        , class "app"
        ]
        [ Common.Content.viewNavbar
        , section []
            [ h1 [] [ text "Gifsync" ]
            , p [] [ text "Add another dimension to gifs by syncing their frames to audio! Still in beta, so things will probably break..." ]
            ]
        , viewUpload model
        , viewResult model
        , Common.Content.standardFooterInfo model.version "https://github.com/dhsavell/gifsync-webapp" |> Common.Content.viewFooter
        ]


filesDecoder : D.Decoder File.File
filesDecoder =
    D.at [ "target", "files" ] (D.index 0 File.decoder)



-- PORTS


port clearVideo : () -> Cmd msg


port setVideo : String -> Cmd msg
