module Api exposing (..)

import Http exposing (jsonBody)
import Json.Encode as E exposing (object)
import Json.Decode as D exposing (Decoder)
import Data.User exposing (User, decodeUser)
import Data.Remote exposing (LoginResult, ErrorStatus, decodeLoginResult, decodeErrorStatus, errorParser)


baseUrl : String
baseUrl =
    "http://localhost:8000/api/v1"


signup : (Result Http.Error User -> msg) -> String -> String -> String -> String -> Cmd msg
signup msg email password firstname lastname =
    let
        body =
            jsonBody <|
                object
                    [ ( "email", E.string email )
                    , ( "password", E.string password )
                    , ( "firstname", E.string firstname )
                    , ( "lastname", E.string lastname )
                    ]
    in
        Http.send msg <| Http.post (baseUrl ++ "/users") body <| D.field "user" decodeUser


login : (Result Http.Error LoginResult -> msg) -> String -> String -> Cmd msg
login msg email password =
    let
        body =
            jsonBody <|
                object
                    [ ( "email", E.string email )
                    , ( "password", E.string password )
                    ]
    in
        Http.send msg <| Http.post (baseUrl ++ "/auth/login") body decodeLoginResult
