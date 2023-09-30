module Base exposing (Effect(..), Model, Msg(..), branch)

import Header
import Home
import Html exposing (Html)
import Html.Attributes
import Login
import Register
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

        ( home, homeEffects ) =
            Home.branch.init context
                |> Sprig.extractModel

        ( login, loginEffects ) =
            Login.branch.init context
                |> Sprig.extractModel

        ( register, registerEffects ) =
            Register.branch.init context
                |> Sprig.extractModel
    in
    { header = header
    , home = home
    , login = login
    , register = register
    }
        |> Sprig.save
        |> Sprig.withChildEffects HeaderMsg applyHeaderEffects headerEffects
        |> Sprig.withChildEffects HomeMsg applyHomeEffects homeEffects
        |> Sprig.withChildEffects LoginMsg applyLoginEffects loginEffects
        |> Sprig.withChildEffects RegisterMsg applyRegisterEffects registerEffects


applyHeaderEffects : Header.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHeaderEffects _ sprig =
    sprig


applyHomeEffects : Home.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHomeEffects _ sprig =
    sprig


applyLoginEffects : Login.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyLoginEffects _ sprig =
    sprig


applyRegisterEffects : Register.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyRegisterEffects _ sprig =
    sprig


type alias Model =
    { header : Header.Model
    , home : Home.Model
    , login : Login.Model
    , register : Register.Model
    }


subscriptions : Sprig.Context (Maybe User) -> Model -> Sub Msg
subscriptions context model =
    Sub.batch
        [ Header.branch.subscriptions context model.header
            |> Sub.map HeaderMsg
        , Home.branch.subscriptions context model.home
            |> Sub.map HomeMsg
        , Login.branch.subscriptions context model.login
            |> Sub.map LoginMsg
        ]


type Msg
    = Login
    | HeaderMsg Header.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg


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
            Home.branch.update context homeMsg model.home
                |> Sprig.mapMsg HomeMsg
                |> Sprig.mapModel (\home -> { model | home = home })
                |> Sprig.applyEffects applyHomeEffects

        LoginMsg loginMsg ->
            Login.branch.update context loginMsg model.login
                |> Sprig.mapMsg LoginMsg
                |> Sprig.mapModel (\login -> { model | login = login })
                |> Sprig.applyEffects applyLoginEffects

        RegisterMsg registerMsg ->
            Register.branch.update context registerMsg model.register
                |> Sprig.mapMsg RegisterMsg
                |> Sprig.mapModel (\register -> { model | register = register })
                |> Sprig.applyEffects applyRegisterEffects


urlChanged : Sprig.Context (Maybe User) -> Model -> Sprig Model Msg Effect
urlChanged context model =
    Header.branch.urlChanged context model.header
        |> Sprig.mapMsg HeaderMsg
        |> Sprig.mapModel (\header -> { model | header = header })
        |> Sprig.applyEffects applyHeaderEffects
        |> Sprig.andThen
            (\m ->
                Home.branch.urlChanged context m.home
                    |> Sprig.mapMsg HomeMsg
                    |> Sprig.mapModel (\home -> { m | home = home })
                    |> Sprig.applyEffects applyHomeEffects
            )
        |> Sprig.andThen
            (\m ->
                Login.branch.urlChanged context m.login
                    |> Sprig.mapMsg LoginMsg
                    |> Sprig.mapModel (\login -> { m | login = login })
                    |> Sprig.applyEffects applyLoginEffects
            )
        |> Sprig.andThen
            (\m ->
                Register.branch.urlChanged context m.register
                    |> Sprig.mapMsg RegisterMsg
                    |> Sprig.mapModel (\register -> { m | register = register })
                    |> Sprig.applyEffects applyRegisterEffects
            )


view : Sprig.Context (Maybe User) -> Model -> Html Msg
view context model =
    Html.div []
        [ Header.branch.view context model.header
            |> Html.map HeaderMsg
        , Home.branch.view context model.home
            |> Html.map HomeMsg
        , Login.branch.view context model.login
            |> Html.map LoginMsg
        , Register.branch.view context model.register
            |> Html.map RegisterMsg
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
