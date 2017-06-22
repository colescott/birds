module Data.Remote exposing (LoginResult, ErrorStatus, decodeLoginResult, decodeErrorStatus, errorParser)

import Json.Decode as D exposing (Decoder)
import Data.User exposing (User, decodeUser)
import Http


type alias LoginResult =
    { user : User
    , token : String
    }


decodeLoginResult : Decoder LoginResult
decodeLoginResult =
    D.map2 LoginResult
        (D.field "user" decodeUser)
        (D.field "token" D.string)


type alias ErrorStatus =
    { code : Int
    , message : Maybe String
    }


decodeErrorStatus : Decoder ErrorStatus
decodeErrorStatus =
    D.map2 ErrorStatus
        (D.field "code" D.int)
        (D.maybe <| D.field "message" D.string)


errorParser : Http.Error -> String
errorParser error =
    case error of
        Http.BadStatus res ->
            case (D.decodeString decodeErrorStatus res.body) of
                Ok error ->
                    Maybe.withDefault "Unknown error." error.message

                Err _ ->
                    "Unknown error."

        Http.Timeout ->
            "Connection timed out... Try again in a bit."

        Http.NetworkError ->
            "Network error... Try again in a bit."

        _ ->
            "Unknown error."
