module Api exposing (words)

import Http
import Json.Decode as D exposing (Decoder)


words : (Result Http.Error (List String) -> msg) -> Cmd msg
words message =
    Http.get
        { url = "words.json"
        , expect = Http.expectJson message wordsDecoder
        }


wordsDecoder : Decoder (List String)
wordsDecoder =
    D.field "words" (D.list D.string)
