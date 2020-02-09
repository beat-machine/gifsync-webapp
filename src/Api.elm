module Api exposing (Payload, submitFiles)

import Http
import Bytes exposing (Bytes)
import File

type alias Payload =
    { audio : File.File
    , gif : File.File
    }

submitFiles : String -> Payload -> (Result String Bytes -> msg) -> Cmd msg
submitFiles apiUrl payload toMsg =
    Http.post
        { url = apiUrl
        , body = Http.multipartBody
            [ Http.filePart "audio" payload.audio
            , Http.filePart "gif" payload.gif
            ]
        , expect = expectVideoBytes toMsg
        }

expectVideoBytes : (Result String Bytes -> msg) -> Http.Expect msg
expectVideoBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err "Failed to process server URL. This is a bug. If you have the time, please report it!"

                Http.Timeout_ ->
                    Err "A timeout occurred while contacting the server. It may be down or under heavy load. Try again in a moment."

                Http.NetworkError_ ->
                    Err "A network error occurred."

                Http.BadStatus_ metadata _ ->
                    Err ("A network error occurred (" ++ String.fromInt metadata.statusCode ++ ").")

                Http.GoodStatus_ _ body ->
                    Ok body
