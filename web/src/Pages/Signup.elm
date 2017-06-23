module Pages.Signup exposing (Model, Msg(..), ParentMsg(..), view, update, init)

import Html exposing (button, br, div, h1, input, text, Html)
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Form.Input as Input
import Http
import Data.User exposing (User)
import Utils exposing ((=>))
import Data.Remote exposing (errorParser)
import Api


type alias Model =
    { email : String
    , password : String
    , firstname : String
    , lastname : String
    , error : Maybe String
    }


type Msg
    = UpdateEmail String
    | UpdatePassword String
    | UpdateFirstname String
    | UpdateLastname String
    | SubmitSignup
    | ResolveSignup (Result Http.Error User)


type ParentMsg
    = Noop
    | SignedUp String String


init : Model
init =
    Model "" "" "" "" Nothing


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

        UpdateFirstname firstname ->
            { model | firstname = firstname }
                => Cmd.none
                => Noop

        UpdateLastname lastname ->
            { model | lastname = lastname }
                => Cmd.none
                => Noop

        SubmitSignup ->
            model
                => Api.signup ResolveSignup model.email model.password model.firstname model.lastname
                => Noop

        ResolveSignup (Ok user) ->
            model
                => Cmd.none
                => SignedUp model.email model.password

        ResolveSignup (Err err) ->
            { model | error = Just <| errorParser err }
                => Cmd.none
                => Noop


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Signup" ]
        , div [] <| Maybe.withDefault [] <| Maybe.map (\x -> [ Alert.danger [ Html.text x ] ]) model.error
        , br [] []
        , Input.email [ Input.onInput <| UpdateEmail, Input.placeholder "Email", Input.value model.email ]
        , br [] []
        , Input.password [ Input.onInput <| UpdatePassword, Input.placeholder "Password", Input.value model.password ]
        , br [] []
        , Input.text [ Input.onInput <| UpdateFirstname, Input.placeholder "Firstname", Input.value model.firstname ]
        , br [] []
        , Input.text [ Input.onInput <| UpdateLastname, Input.placeholder "Lastname", Input.value model.lastname ]
        , br [] []
        , Button.button [ Button.onClick SubmitSignup ] [ Html.text "Signup" ]
        ]


onLoad : Model -> ( Model, Cmd Msg )
onLoad model =
    init
        => Cmd.none
