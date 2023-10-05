module Login exposing
    ( Effect
    , InternalModel
    , Model
    , Msg
    , branch
    )

import Api
import Context exposing (Context)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Port
import Tea


type alias LoginTea =
    Context.MyTea InternalModel Msg Effect


branch : Context.Route InternalModel Msg Effect
branch =
    Tea.branch
        { path = [ "login" ]
        }
        { init = init
        , subscriptions = subscriptions
        , update = update
        , urlChanged = urlChanged
        , view = view
        }


type alias Model =
    Tea.RouteModel InternalModel


type alias InternalModel =
    { email : String
    , password : String
    , errors : List String
    }


type alias Effect =
    Never


init : Context -> LoginTea
init _ =
    { email = ""
    , password = ""
    , errors = []
    }
        |> Tea.save


subscriptions : Context -> InternalModel -> Sub Msg
subscriptions _ _ =
    Sub.none


type Msg
    = EmailChanged String
    | PasswordChanged String
    | Login
    | LoginResponded (Result Http.Error Api.UserResponse)


update : Context -> Msg -> InternalModel -> LoginTea
update _ msg model =
    case msg of
        EmailChanged email ->
            { model | email = email }
                |> Tea.save

        PasswordChanged password ->
            { model | password = password }
                |> Tea.save

        Login ->
            model
                |> Tea.save
                |> Tea.withCmd
                    (Api.login
                        { body = { user = { email = model.email, password = model.password } }
                        , toMsg = LoginResponded
                        }
                    )

        LoginResponded (Err _) ->
            { model
                | errors = [ "Login failed" ]
            }
                |> Tea.save

        LoginResponded (Ok { user }) ->
            model
                |> Tea.save
                |> Tea.setFlags (Just user)
                |> Tea.withCmd (Port.storeUser user)
                |> Tea.navigate "/"


urlChanged : Context -> InternalModel -> LoginTea
urlChanged _ model =
    model
        |> Tea.save


view : Context -> InternalModel -> Html Msg
view _ model =
    Html.div [ Html.Attributes.class "auth-page" ]
        [ Html.div [ Html.Attributes.class "container page" ]
            [ Html.div [ Html.Attributes.class "row" ]
                [ Html.div [ Html.Attributes.class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.h1 [ Html.Attributes.class "text-xs-center" ] [ Html.text "Sign in" ]
                    , Html.p [ Html.Attributes.class "text-xs-center" ]
                        [ Html.a [ Html.Attributes.href "/register" ] [ Html.text "Need an account?" ]
                        ]
                    , model.errors
                        |> List.map (\error -> Html.li [] [ Html.text error ])
                        |> Html.ul [ Html.Attributes.class "error-messages" ]
                    , Html.form [ Html.Events.onSubmit Login ]
                        [ Html.fieldset [ Html.Attributes.class "form-group" ]
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
                            [ Html.text "Sign in" ]
                        ]
                    ]
                ]
            ]
        ]
