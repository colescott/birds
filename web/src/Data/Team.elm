module Team exposing (Team, Prerequisite, decodeTeam, encodeTeam)

import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)


type alias Team =
    { id_ : String
    , name : String
    , teamnumber : Int
    , password : Maybe String
    }


decodeTeam : Decoder Team
decodeTeam =
    D.map4 Team
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "teamnumber" D.int)
        (D.maybe <| D.field "password" D.string)


encodeTeam : Team -> Value
encodeTeam team =
    E.object
        [ ( "id", E.string team.id_ )
        , ( "name", E.string team.name )
        , ( "teamnumber", E.int team.teamnumber )
        , ( "password", Maybe.withDefault E.null <| Maybe.map E.string team.password )
        ]


type alias Prerequisite =
    { id_ : String }


decodePrerequisite : Decoder Prerequisite
decodePrerequisite =
    D.map Prerequisite
        (D.field "id" D.string)


encodePrerequisite : Prerequisite -> Value
encodePrerequisite prerequisite =
    E.object [ ( "id", E.string prerequisite.id_ ) ]
