module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, string)
import Json.Encode as E
import Data.Remote
import Http
import Dict


suite : Test
suite =
    Test.concat
        [ describe "Api Tests"
            [ describe "errorParser"
                [ fuzz2 string int "BadStatus should return message" <|
                    \message code ->
                        Http.BadStatus (Http.Response "/test/url" { code = code, message = message } (Dict.fromList []) <| E.encode 0 <| encodeErrorStatus <| Data.Remote.ErrorStatus code (Just message))
                            |> Data.Remote.errorParser
                            |> Expect.equal message
                , test "Timeout should return default timeout message" <|
                    \_ ->
                        Http.Timeout
                            |> Data.Remote.errorParser
                            |> Expect.equal "Connection timed out... Try again in a bit."
                , test "NetworkError should return default network error message" <|
                    \_ ->
                        Http.NetworkError
                            |> Data.Remote.errorParser
                            |> Expect.equal "Network error... Try again in a bit."
                , test "BadUrl should return default message" <|
                    \_ ->
                        Http.BadUrl "/test/url"
                            |> Data.Remote.errorParser
                            |> Expect.equal "Unknown error."
                , fuzz2 string int "BadPayload should return default message" <|
                    \message code ->
                        Http.BadPayload "that payload was bad." (Http.Response "/test/url" { code = code, message = message } (Dict.fromList []) <| E.encode 0 <| encodeErrorStatus <| Data.Remote.ErrorStatus code (Just message))
                            |> Data.Remote.errorParser
                            |> Expect.equal "Unknown error."
                ]
            ]
        ]


encodeErrorStatus : Data.Remote.ErrorStatus -> E.Value
encodeErrorStatus err =
    E.object
        [ ( "code", E.int err.code )
        , ( "message", E.string (Maybe.withDefault "Unknown error :/" err.message) )
        ]
