module Api exposing (Payload, submitFiles)

import Http
import Bytes exposing (Bytes)
import Common.HttpExtras
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
        , expect = Common.HttpExtras.expectRawBytes toMsg
        }
