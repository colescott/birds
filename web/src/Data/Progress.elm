module Data.Progress exposing (Progress, decodeProgress, encodeProgress)

import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as E


type alias Progress =
    { id_ : String
    , state : String
    }


decodeProgress : Decoder Progress
decodeProgress =
    D.map2 Progress
        (D.field "id" D.string)
        (D.field "state" D.string)


encodeProgress : Progress -> Value
encodeProgress progress =
    E.object
        [ ( "id", E.string progress.id_ )
        , ( "state", E.string progress.state )
        ]
