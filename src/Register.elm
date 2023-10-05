module Register exposing
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
import Tea exposing (Tea)


type alias RegisterTea =
    Context.MyTea InternalModel Msg Effect


branch : Context.Route InternalModel Msg Effect
branch =
    Tea.branch
        { path = [ "register" ]
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
    { username : String
    , email : String
    , password : String
    , errors : List String
    }


type alias Effect =
    Never


init : Context -> RegisterTea
init context =
    { username = ""
    , email = ""
    , password = ""
    , errors = []
    }
        |> Tea.save


subscriptions : Context -> InternalModel -> Sub Msg
subscriptions _ _ =
    Sub.none


type Msg
    = UsernameChanged String
    | EmailChanged String
    | PasswordChanged String
      -- | LoginSuccess User
      -- | LoginFailure String
    | Register
    | Registered (Result Http.Error Api.UserResponse)


update : Context -> Msg -> InternalModel -> RegisterTea
update context msg model =
    case msg of
        UsernameChanged username ->
            { model | username = username }
                |> Tea.save

        EmailChanged email ->
            { model | email = email }
                |> Tea.save

        PasswordChanged password ->
            { model | password = password }
                |> Tea.save

        Register ->
            model
                |> Tea.save
                |> Tea.withCmd
                    (Api.createUser
                        { body =
                            { user =
                                { email = model.email
                                , password = model.password
                                , username = model.username
                                }
                            }
                        , toMsg = Registered
                        }
                    )

        Registered (Err err) ->
            { model
                | errors = [ Debug.toString err ]
            }
                |> Tea.save

        Registered (Ok { user }) ->
            model
                |> Tea.save
                |> Tea.setFlags (Just user)
                |> Tea.navigate "/"


urlChanged : Context -> InternalModel -> RegisterTea
urlChanged _ model =
    model
        |> Tea.save


view : Context -> InternalModel -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "auth-page" ]
        [ Html.div [ Html.Attributes.class "container page" ]
            [ Html.div [ Html.Attributes.class "row" ]
                [ Html.div [ Html.Attributes.class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.h1 [ Html.Attributes.class "text-xs-center" ] [ Html.text "Sign up" ]
                    , Html.p [ Html.Attributes.class "text-xs-center" ]
                        [ Html.a [ Html.Attributes.href "/login" ] [ Html.text "Have an account?" ]
                        ]
                    , Html.ul [ Html.Attributes.class "error-messages" ]
                        (List.map
                            (\error ->
                                Html.li [] [ Html.text error ]
                            )
                            model.errors
                        )
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
