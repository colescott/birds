module Data.User exposing (User, decodeUser, encodeUser)

import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as E
import Data.Progress exposing (..)


type alias User =
    { id_ : String
    , email : String
    , firstname : String
    , lastname : String
    , progress : List Progress
    }


decodeUser : Decoder User
decodeUser =
    D.map5 User
        (D.field "id" D.string)
        (D.field "email" D.string)
        (D.field "firstname" D.string)
        (D.field "lastname" D.string)
        (D.map (Maybe.withDefault []) <| D.maybe <| D.field "progress" <| D.list decodeProgress)


encodeUser : User -> Value
encodeUser user =
    E.object
        [ ( "id", E.string user.id_ )
        , ( "email", E.string user.email )
        , ( "firstname", E.string user.firstname )
        , ( "lastname", E.string user.lastname )
        , ( "progress", E.list <| List.map encodeProgress user.progress )
        ]
