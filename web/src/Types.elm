module Types exposing (Msg(..), Notification(..), Model)

import Router exposing (Route)
import Navigation exposing (Location)
import Bootstrap.Navbar as Navbar
import Data.User exposing (User)
import Pages.Signup
import Pages.Login
import Http
import Data.Remote exposing (LoginResult)
import Array exposing (Array)


type alias Model =
    { route : Route
    , user : Maybe User
    , token : Maybe String
    , navbarState : Navbar.State
    , signup : Pages.Signup.Model
    , login : Pages.Login.Model
    , notifications : Array Notification
    }


type Msg
    = LocationChanged Location
    | NavbarMsg Navbar.State
    | SignupPage Pages.Signup.Msg
    | LoginPage Pages.Login.Msg
    | SignupLogin (Result Http.Error LoginResult)
    | AddError Notification
    | RemoveError Int


type Notification
    = Error String
    | Warning String
    | Info String
    | Success String
