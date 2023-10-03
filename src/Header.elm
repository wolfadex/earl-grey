module Header exposing (Effect(..), Model, Msg(..), branch)

import Api
import AppUrl
import Context exposing (Context)
import Dict
import Html exposing (Html)
import Html.Attributes
import Tea exposing (Tea)


branch : Context.Branch Model Msg Effect
branch =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Context -> Tea Model Msg Effect
init _ =
    {}
        |> Tea.save


type alias Model =
    {}


subscriptions : Context -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


type Msg
    = Login


type Effect
    = Navigate


update : Context -> Msg -> Model -> Tea Model Msg Effect
update _ msg model =
    case msg of
        Login ->
            model
                |> Tea.save
                |> Tea.withEffect Navigate


urlChanged : Context -> Model -> Tea Model Msg Effect
urlChanged _ _ =
    {}
        |> Tea.save


view : Context -> Model -> Html Msg
view context _ =
    Html.nav [ Html.Attributes.class "navbar navbar-light" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.a [ Html.Attributes.class "navbar-brand", Html.Attributes.href "/" ]
                [ Html.text "conduit" ]
            , Html.ul
                [ Html.Attributes.class "nav navbar-nav pull-xs-right" ]
                ([ ( True, link (Tea.absolutePath context) { label = Html.text "Home", path = [] } )
                 , ( Tea.flags context == Nothing
                   , link (Tea.absolutePath context)
                        { label = Html.text "Sign in"
                        , path = [ "login" ]
                        }
                   )
                 , ( Tea.flags context == Nothing
                   , link (Tea.absolutePath context)
                        { label = Html.text "Sign up"
                        , path = [ "register" ]
                        }
                   )
                 , ( Tea.flags context /= Nothing
                   , link (Tea.absolutePath context)
                        { label =
                            Html.div []
                                [ Html.i [ Html.Attributes.class "ion-compose" ] []
                                , Html.text "&nbsp;New Article"
                                ]
                        , path = [ "editor" ]
                        }
                   )
                 , ( Tea.flags context /= Nothing
                   , link (Tea.absolutePath context)
                        { label =
                            Html.div []
                                [ Html.i [ Html.Attributes.class "ion-gear-a" ] []
                                , Html.text "&nbsp;Settings"
                                ]
                        , path = [ "settings" ]
                        }
                   )
                 , case Tea.flags context of
                    Nothing ->
                        ( False, Html.text "" )

                    Just user ->
                        ( True
                        , link (Tea.absolutePath context)
                            { label = Html.text user.username
                            , path =
                                [ "profile"
                                , user.username
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
