module Login exposing
    ( Effect
    , Model
    , Msg
    , branch
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Branch (Maybe User) Model Msg Effect
branch =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


type alias Model =
    { email : String
    , password : String
    }


type alias Effect =
    Never


init : Sprig.Context (Maybe User) -> Sprig Model Msg Effect
init context =
    { email = "", password = "" }
        |> Sprig.save


subscriptions : Sprig.Context (Maybe User) -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


type Msg
    = EmailChanged String
    | Login
      -- | LoginSuccess User
      -- | LoginFailure String
    | PasswordChanged String


update : Sprig.Context (Maybe User) -> Msg -> Model -> Sprig Model Msg Effect
update context msg model =
    case msg of
        EmailChanged email ->
            { model | email = email }
                |> Sprig.save

        PasswordChanged password ->
            { model | password = password }
                |> Sprig.save

        Login ->
            model
                |> Sprig.save


urlChanged : Sprig.Context (Maybe User) -> Model -> Sprig Model Msg Effect
urlChanged _ model =
    model
        |> Sprig.save


view : Sprig.Context (Maybe User) -> Model -> Html Msg
view context model =
    {-
       <div class="col-md-6 offset-md-3 col-xs-12">
           <h1 class="text-xs-center">Sign in</h1>
           <p class="text-xs-center">
           <a href="/register">Need an account?</a>
           </p>

           <ul class="error-messages">
           <li>That email is already taken</li>
           </ul>

           <form>
           <fieldset class="form-group">
               <input class="form-control form-control-lg" type="text" placeholder="Email" />
           </fieldset>
           <fieldset class="form-group">
               <input class="form-control form-control-lg" type="password" placeholder="Password" />
           </fieldset>
           <button class="btn btn-lg btn-primary pull-xs-right">Sign in</button>
           </form>
       </div>
    -}
    Html.div [ Html.Attributes.class "auth-page" ]
        [ Html.div [ Html.Attributes.class "container page" ]
            [ Html.div [ Html.Attributes.class "row" ]
                [ Html.div [ Html.Attributes.class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.h1 [ Html.Attributes.class "text-xs-center" ] [ Html.text "Sign in" ]
                    , Html.p [ Html.Attributes.class "text-xs-center" ]
                        [ Html.text "Need an account?"
                        , Html.a [ Html.Attributes.href "/register" ] [ Html.text "Need an account?" ]
                        ]
                    , Html.ul [ Html.Attributes.class "error-messages" ]
                        [ Html.li [] [ Html.text "That email is already taken" ] ]
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
