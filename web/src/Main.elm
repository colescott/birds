module Main exposing (..)

import Html exposing (div, Html)
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Router exposing (genRoute, Route(Home, Login, Signup, NotFound))
import Api
import Pages.Home
import Pages.Login
import Pages.Signup
import Pages.NotFound
import Maybe
import Components.Menu
import Components.Notifications
import Types exposing (Msg(..), Model)
import Navigation exposing (Location)
import Utils exposing ((=>))
import Array exposing (Array)
import Data.Remote


main : Program Never Model Msg
main =
    Navigation.program LocationChanged
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        { route = genRoute location
        , user = Nothing
        , token = Nothing
        , navbarState = navbarState
        , login = Pages.Login.init
        , signup = Pages.Signup.init
        , notifications = Array.empty
        }
            => navbarCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChanged location ->
            let
                route =
                    genRoute location

                newModel =
                    { model | route = route }
            in
                case route of
                    Router.Login ->
                        let
                            ( newState, cmd ) =
                                Pages.Login.onLoad model.login
                        in
                            { newModel | login = newState }
                                => Cmd.map Types.LoginPage cmd

                    _ ->
                        newModel
                            => Cmd.none

        NavbarMsg state ->
            { model | navbarState = state }
                => Cmd.none

        Types.SignupPage msg ->
            let
                ( ( newState, cmd ), parentMsg ) =
                    Pages.Signup.update msg model.signup
            in
                case parentMsg of
                    Pages.Signup.Noop ->
                        { model | signup = newState }
                            => Cmd.map Types.SignupPage cmd

                    Pages.Signup.SignedUp email password ->
                        { model | signup = newState }
                            => Cmd.batch
                                [ Cmd.map Types.SignupPage cmd
                                , Api.login Types.SignupLogin email password
                                ]

        Types.LoginPage msg ->
            let
                ( ( newState, cmd ), parentMsg ) =
                    Pages.Login.update msg model.login
            in
                case parentMsg of
                    Pages.Login.Noop ->
                        { model | login = newState }
                            => Cmd.map Types.LoginPage cmd

                    Pages.Login.SetUser user token ->
                        { model | login = newState, user = Just user, token = Just token }
                            => Cmd.batch
                                [ Navigation.newUrl "/"
                                , Cmd.map Types.LoginPage cmd
                                ]

        Types.SignupLogin (Ok { user, token }) ->
            { model | user = Just user, token = Just token }
                => Navigation.newUrl "/"

        Types.SignupLogin (Err err) ->
            { model | notifications = Array.push (Types.Error <| Data.Remote.errorParser err) model.notifications }
                => Cmd.none

        AddError err ->
            { model | notifications = Array.push err model.notifications }
                => Cmd.none

        RemoveError i ->
            { model | notifications = Array.append (Array.slice 0 i model.notifications) (Array.slice (i + 1) (Array.length model.notifications) model.notifications) }
                => Cmd.none


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
          -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ Components.Menu.menu model.user model.navbarState
                , Components.Notifications.notifications model.notifications
                , router model
                ]
            ]
        ]


router : Model -> Html Msg
router model =
    case model.route of
        Home ->
            Pages.Home.view model

        Router.Login ->
            Html.map Types.LoginPage <| Pages.Login.view model.login

        Router.Signup ->
            Html.map Types.SignupPage <| Pages.Signup.view model.signup

        NotFound ->
            Pages.NotFound.view model


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg
