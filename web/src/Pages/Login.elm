module Pages.Login exposing (Model, Msg, ParentMsg(..), view, init, update, onLoad)

import Html exposing (br, div, h1, h3, input, text, Html)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Data.User exposing (User)
import Http
import Data.Remote exposing (LoginResult)
import Utils exposing ((=>))
import Api
import Data.Remote exposing (errorParser)


type alias Model =
    { email : String
    , password : String
    , error : Maybe String
    }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | UpdateError (Maybe String)
    | Login
    | LoginFinished (Result Http.Error LoginResult)


type ParentMsg
    = Noop
    | SetUser User String


init : Model
init =
    Model "" "" Nothing


update : Msg -> Model -> ( ( Model, Cmd Msg ), ParentMsg )
update msg model =
    case msg of
        UpdateEmail email ->
            { model | email = email }
                => Cmd.none
                => Noop

        UpdatePassword password ->
            { model | password = password }
                => Cmd.none
                => Noop

        UpdateError error ->
            { model | error = error }
                => Cmd.none
                => Noop

        Login ->
            { model | error = Nothing }
                => Api.login LoginFinished model.email model.password
                => Noop

        LoginFinished (Ok { user, token }) ->
            model
                => Cmd.none
                => SetUser user token

        LoginFinished (Err error) ->
            { model | error = Just <| errorParser error }
                => Cmd.none
                => Noop


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text "Login" ]
        , div [] <| Maybe.withDefault [] <| Maybe.map (\x -> [ Alert.danger [ Html.text x ] ]) model.error
        , br [] []
        , Input.email [ Input.onInput <| UpdateEmail, Input.placeholder "Email", Input.value model.email ]
        , br [] []
        , Input.password [ Input.onInput <| UpdatePassword, Input.placeholder "Password", Input.value model.password ]
        , br [] []
        , Button.button [ Button.onClick Login ] [ Html.text "Login" ]
        ]


onLoad : Model -> ( Model, Cmd Msg )
onLoad model =
    init
        => Cmd.none
