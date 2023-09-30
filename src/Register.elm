module Register exposing
    ( Effect
    , InternalModel
    , Model
    , Msg
    , branch
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Route (Maybe User) InternalModel Msg Effect
branch =
    Sprig.branch
        { path = [ "register" ]
        }
        { init = init
        , subscriptions = subscriptions
        , update = update
        , urlChanged = urlChanged
        , view = view
        }


type alias Model =
    Sprig.RouteModel InternalModel


type alias InternalModel =
    { username : String
    , email : String
    , password : String
    }


type alias Effect =
    Never


init : Sprig.Context (Maybe User) -> Sprig InternalModel Msg Effect
init context =
    { username = ""
    , email = ""
    , password = ""
    }
        |> Sprig.save


subscriptions : Sprig.Context (Maybe User) -> InternalModel -> Sub Msg
subscriptions _ _ =
    Sub.none


type Msg
    = UsernameChanged String
    | EmailChanged String
    | PasswordChanged String
      -- | LoginSuccess User
      -- | LoginFailure String
    | Register


update : Sprig.Context (Maybe User) -> Msg -> InternalModel -> Sprig InternalModel Msg Effect
update context msg model =
    case msg of
        UsernameChanged username ->
            { model | username = username }
                |> Sprig.save

        EmailChanged email ->
            { model | email = email }
                |> Sprig.save

        PasswordChanged password ->
            { model | password = password }
                |> Sprig.save

        Register ->
            model
                |> Sprig.save


urlChanged : Sprig.Context (Maybe User) -> InternalModel -> Sprig InternalModel Msg Effect
urlChanged _ model =
    model
        |> Sprig.save


view : Sprig.Context (Maybe User) -> InternalModel -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "auth-page" ]
        [ Html.div [ Html.Attributes.class "container page" ]
            [ Html.div [ Html.Attributes.class "row" ]
                [ Html.div [ Html.Attributes.class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.h1 [ Html.Attributes.class "text-xs-center" ] [ Html.text "Sign up" ]
                    , Html.p [ Html.Attributes.class "text-xs-center" ]
                        [ Html.a [ Html.Attributes.href "/login" ] [ Html.text "Have an account?" ]
                        ]

                    -- , Html.ul [ Html.Attributes.class "error-messages" ]
                    --     [ Html.li [] [ Html.text "That email is already taken" ] ]
                    , Html.form [ Html.Events.onSubmit Register ]
                        [ Html.fieldset [ Html.Attributes.class "form-group" ]
                            [ Html.input
                                [ Html.Attributes.class "form-control form-control-lg"
                                , Html.Attributes.type_ "text"
                                , Html.Attributes.placeholder "Username"
                                , Html.Events.onInput UsernameChanged
                                , Html.Attributes.value model.username
                                ]
                                []
                            ]
                        , Html.fieldset [ Html.Attributes.class "form-group" ]
                            [ Html.input
                                [ Html.Attributes.class "form-control form-control-lg"
                                , Html.Attributes.type_ "text"
                                , Html.Attributes.placeholder "Email"
                                , Html.Events.onInput EmailChanged
                                , Html.Attributes.value model.email
                                ]
                                []
                            ]
                        , Html.fieldset [ Html.Attributes.class "form-group" ]
                            [ Html.input
                                [ Html.Attributes.class "form-control form-control-lg"
                                , Html.Attributes.type_ "password"
                                , Html.Attributes.placeholder "Password"
                                , Html.Events.onInput PasswordChanged
                                , Html.Attributes.value model.password
                                ]
                                []
                            ]
                        , Html.button
                            [ Html.Attributes.class "btn btn-lg btn-primary pull-xs-right"
                            , Html.Attributes.type_ "submit"
                            ]
                            [ Html.text "Sign up" ]
                        ]
                    ]
                ]
            ]
        ]
