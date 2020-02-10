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
    | Waiting
    | Done


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
                    ( { model | status = Waiting }
                    , Cmd.batch
                        [ clearVideo ()
                        , Api.submitFiles model.apiUrl { gif = gifFile, audio = audioFile } GotVideo
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotVideo (Err error) ->
            ( { model | status = Idle }, Cmd.none )

        GotVideo (Ok videoBytes) ->
            case Base64.fromBytes videoBytes of
                Just d ->
                    ( { model | status = Done }, setVideo d )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewUpload : Model -> Html Msg
viewUpload model =
    section [ class "row" ]
        [ div [ class "frame six columns upload-panel" ]
            [ h3 [] [ text "Gif" ]
            , p [] [ text "Upload a gif." ]
            , input
                [ type_ "file"
                , multiple False
                , accept "image/gif, .gif"
                , on "change" (D.map SetGif filesDecoder)
                ]
                []
            ]
        , div [ class "frame six columns upload-panel" ]
            [ h3 [] [ text "Audio" ]
            , p [] [ text "Upload an mp3 up to 2:30 in length." ]
            , input
                [ type_ "file"
                , multiple False
                , accept "audio/mpeg, .mp3"
                , on "change" (D.map SetAudio filesDecoder)
                ]
                []
            ]
        ]


viewResult : Model -> Html Msg
viewResult model =
    section [ class "frame" ]
        [ h3 [] [ text "Result" ]
        , p [] [ text "Press the button below to render the result!" ]
        , button [ class "button-primary render-button", disabled (model.audio == Nothing || model.gif == Nothing), onClick Submit ] [ text "Submit!" ]
        , video [ classList [ ("hidden", model.status /= Done) ], id "player", controls True, autoplay False ] []
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
            , p [] [ text "Sync gifs and audio. See for yourself!" ]
            ]
        , viewUpload model
        , Common.Content.viewPatreonSection NoOp "Gifsync"
        , viewResult model
        , Common.Content.standardFooterInfo model.version "https://github.com/dhsavell/gifsync-webapp" |> Common.Content.viewFooter
        ]


filesDecoder : D.Decoder File.File
filesDecoder =
    D.at [ "target", "files" ] (D.index 0 File.decoder)



-- PORTS


port clearVideo : () -> Cmd msg


port setVideo : String -> Cmd msg
