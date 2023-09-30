module Base exposing (Effect(..), Model, Msg(..), branch)

import Header
import Home
import Html exposing (Html)
import Html.Attributes
import Login
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


init : Sprig.Context (Maybe User) -> Sprig Model Msg Effect
init context =
    let
        ( header, headerEffects ) =
            Header.branch.init context
                |> Sprig.extractModel

        applyRoute =
            case Sprig.absolutePath context of
                [] ->
                    let
                        ( home, homeEffects ) =
                            Home.branch.init context
                                |> Sprig.extractModel
                    in
                    Sprig.mapModel (\model -> { model | home = Just home })
                        >> Sprig.withChildEffects HomeMsg applyHomeEffects homeEffects

                [ "login" ] ->
                    let
                        ( login, loginEffects ) =
                            Login.branch.init context
                                |> Sprig.extractModel
                    in
                    Sprig.mapModel (\model -> { model | login = Just login })
                        >> Sprig.withChildEffects LoginMsg applyLoginEffects loginEffects

                _ ->
                    Debug.todo ""
    in
    { header = header
    , home = Nothing
    , login = Nothing
    }
        |> Sprig.save
        |> Sprig.withChildEffects HeaderMsg applyHeaderEffects headerEffects
        |> applyRoute



-- |> Sprig.withChildEffects routeMsgMap applyRouteEffs routeEffects


applyHeaderEffects : Header.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHeaderEffects _ sprig =
    sprig


applyHomeEffects : Home.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHomeEffects _ sprig =
    sprig


applyLoginEffects : Login.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyLoginEffects _ sprig =
    sprig


type alias Model =
    { header : Header.Model
    , home : Maybe Home.Model
    , login : Maybe Login.Model
    }


subscriptions : Sprig.Context (Maybe User) -> Model -> Sub Msg
subscriptions context model =
    Sub.batch
        [ Header.branch.subscriptions context model.header
            |> Sub.map HeaderMsg
        , model.home
            |> Maybe.map (Home.branch.subscriptions context >> Sub.map HomeMsg)
            |> Maybe.withDefault Sub.none
        , model.login
            |> Maybe.map (Login.branch.subscriptions context >> Sub.map LoginMsg)
            |> Maybe.withDefault Sub.none
        ]


type Msg
    = Login
    | HeaderMsg Header.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg


type Effect
    = Navigate (List String)


update : Sprig.Context (Maybe User) -> Msg -> Model -> Sprig Model Msg Effect
update context msg model =
    case msg of
        Login ->
            model
                |> Sprig.save
                |> Sprig.withEffect (Navigate [ "login" ])

        HeaderMsg headerMsg ->
            Header.branch.update context headerMsg model.header
                |> Sprig.mapMsg HeaderMsg
                |> Sprig.mapModel (\header -> { model | header = header })
                |> Sprig.applyEffects applyHeaderEffects

        HomeMsg homeMsg ->
            case model.home of
                Nothing ->
                    model
                        |> Sprig.save

                Just home_ ->
                    Home.branch.update context homeMsg home_
                        |> Sprig.mapMsg HomeMsg
                        |> Sprig.mapModel (\home -> { model | home = Just home })
                        |> Sprig.applyEffects applyHomeEffects

        LoginMsg loginMsg ->
            case model.login of
                Nothing ->
                    model
                        |> Sprig.save

                Just login_ ->
                    Login.branch.update context loginMsg login_
                        |> Sprig.mapMsg LoginMsg
                        |> Sprig.mapModel (\login -> { model | login = Just login })
                        |> Sprig.applyEffects applyLoginEffects


urlChanged : Sprig.Context (Maybe User) -> Model -> Sprig Model Msg Effect
urlChanged context model =
    Header.branch.urlChanged context model.header
        |> Sprig.mapMsg HeaderMsg
        |> Sprig.mapModel (\header -> { model | header = header })
        |> Sprig.applyEffects applyHeaderEffects


view : Sprig.Context (Maybe User) -> Model -> Html Msg
view context model =
    Html.div []
        [ Header.branch.view context model.header
            |> Html.map HeaderMsg
        , case Sprig.absolutePath context of
            [] ->
                model.home
                    |> Maybe.map (Home.branch.view context >> Html.map HomeMsg)
                    |> Maybe.withDefault (Html.text "Loading home...")

            [ "login" ] ->
                model.login
                    |> Maybe.map (Login.branch.view context >> Html.map LoginMsg)
                    |> Maybe.withDefault (Html.text "Loading login...")

            _ ->
                Html.text "404"
        , viewFooter
        ]


viewFooter : Html Msg
viewFooter =
    Html.footer []
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.a [ Html.Attributes.href "/", Html.Attributes.class "logo-font" ] [ Html.text "conduit" ]
            , Html.span [ Html.Attributes.class "attribution" ]
                [ Html.text "An interactive learning project from "
                , Html.a [ Html.Attributes.href "https :// thinkster.io" ] [ Html.text "Thinkster" ]
                , Html.text ". Code design licensed under MIT."
                ]
            ]
        ]
