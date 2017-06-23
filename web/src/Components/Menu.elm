module Components.Menu exposing (menu)

import Html exposing (a, div, text, ul, li, Html)
import Html.Attributes exposing (href, class, style)
import Bootstrap.Navbar as Navbar exposing (State)
import Types exposing (Msg(NavbarMsg))
import Data.User exposing (User)


menu : Maybe User -> State -> Html Msg
menu user model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.attrs [ style [ ( "margin-bottom", "10px" ) ] ]
        |> Navbar.brand [ href "#" ] [ text "Birds" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Learn" ]
            , Navbar.itemLink [ href "#" ] [ text "Profile" ]
            , Navbar.itemLink [ href "#" ] [ text "Team" ]
            ]
        |> Navbar.customItems
            -- This is some bullshit to make it float right... *sigh*
            [ customItems user ]
        |> Navbar.view model


customItems : Maybe User -> Navbar.CustomItem msg
customItems user =
    case user of
        Nothing ->
            userNotLoggedIn

        Just _ ->
            userLoggedIn


userNotLoggedIn : Navbar.CustomItem msg
userNotLoggedIn =
    Navbar.customItem <|
        ul [ class "navbar-nav" ]
            [ li [ class "nav-item" ] [ a [ href "#login", class "nav-link" ] [ text "Login" ] ]
            , li [ class "nav-item" ] [ a [ href "#signup", class "nav-link" ] [ text "Sign Up" ] ]
            ]


userLoggedIn : Navbar.CustomItem msg
userLoggedIn =
    Navbar.customItem <|
        ul [ class "navbar-nav" ]
            [ li [ class "nav-item" ] [ a [ href "#", class "nav-link" ] [ text "Sign Out" ] ]
            ]
