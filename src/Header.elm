module Header exposing (Effect(..), Model, Msg(..), branch)

import AppUrl
import Dict
import Html exposing (Html)
import Html.Attributes
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Branch (Maybe User) Model Msg Effect
branch =
    { init = init
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Sprig.Context (Maybe User) -> Sprig Model Msg Effect
init _ =
    {}
        |> Sprig.save


type alias Model =
    {}


type Msg
    = Login


type Effect
    = Navigate


update : Sprig.Context (Maybe User) -> Msg -> Model -> Sprig Model Msg Effect
update _ msg model =
    case msg of
        Login ->
            model
                |> Sprig.save
                |> Sprig.withEffect Navigate


urlChanged : Sprig.Context (Maybe User) -> Model -> Sprig Model Msg Effect
urlChanged _ _ =
    {}
        |> Sprig.save


view : Sprig.Context (Maybe User) -> Model -> Html Msg
view context _ =
    Html.nav [ Html.Attributes.class "navbar navbar-light" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.a [ Html.Attributes.class "navbar-brand", Html.Attributes.href "/" ]
                [ Html.text "conduit" ]
            , Html.ul
                [ Html.Attributes.class "nav navbar-nav pull-xs-right" ]
                ([ ( True, link (Sprig.absolutePath context) { label = Html.text "Home", path = [] } )
                 , ( Sprig.flags context == Nothing
                   , link (Sprig.absolutePath context)
                        { label = Html.text "Sign in"
                        , path = [ "login" ]
                        }
                   )
                 , ( Sprig.flags context == Nothing
                   , link (Sprig.absolutePath context)
                        { label = Html.text "Sign up"
                        , path = [ "register" ]
                        }
                   )
                 , ( Sprig.flags context /= Nothing
                   , link (Sprig.absolutePath context)
                        { label =
                            Html.div []
                                [ Html.i [ Html.Attributes.class "ion-compose" ] []
                                , Html.text "&nbsp;New Article"
                                ]
                        , path = [ "editor" ]
                        }
                   )
                 , ( Sprig.flags context /= Nothing
                   , link (Sprig.absolutePath context)
                        { label =
                            Html.div []
                                [ Html.i [ Html.Attributes.class "ion-gear-a" ] []
                                , Html.text "&nbsp;Settings"
                                ]
                        , path = [ "settings" ]
                        }
                   )
                 , case Sprig.flags context of
                    Nothing ->
                        ( False, Html.text "" )

                    Just user ->
                        ( True
                        , link (Sprig.absolutePath context)
                            { label = Html.text (User.username user)
                            , path =
                                [ "profile"
                                , User.username user
                                    |> String.toLower
                                    |> String.replace " " "-"
                                ]
                            }
                        )
                 ]
                    |> List.filter Tuple.first
                    |> List.map Tuple.second
                )
            ]
        ]


link : List String -> { label : Html Msg, path : List String } -> Html Msg
link absolutePath options =
    Html.li
        [ Html.Attributes.class "nav-item" ]
        [ Html.a
            [ Html.Attributes.class (linkClass absolutePath options.path)
            , Html.Attributes.href
                (AppUrl.toString
                    { path = options.path
                    , queryParameters = Dict.empty
                    , fragment = Nothing
                    }
                )
            ]
            [ options.label ]
        ]


linkClass : List String -> List String -> String
linkClass absolutePath route =
    if absolutePath == route then
        "nav-link active"

    else
        "nav-link"
