module Pages.Home exposing (view)

import Html exposing (div, h1, text, Html)


view : model -> Html msg
view _ =
    div [] [ h1 [] [ text "Home" ] ]
