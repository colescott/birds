module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (s, top, (</>), map, oneOf, parseHash, Parser)


type Route
    = Home
    | Login
    | Signup
    | NotFound


router : Parser (Route -> c) c
router =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Signup (s "signup")
        ]


genRoute : Location -> Route
genRoute location =
    Maybe.withDefault NotFound <| parseHash router location
