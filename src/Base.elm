module Base exposing (Effect, Model, Msg(..), branch)

import Context exposing (Context)
import Header
import Home
import Html exposing (Html)
import Html.Attributes
import Login
import Register
import Tea


type alias BaseTea =
    Context.MyTea Model Msg Effect


branch : Context.Branch Model Msg Effect
branch =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Context -> BaseTea
init context =
    let
        ( header, headerEffects ) =
            Header.branch.init context
                |> Tea.extractModel

        ( home, homeEffects ) =
            Home.branch.init context
                |> Tea.extractModel

        ( login, loginEffects ) =
            Login.branch.init context
                |> Tea.extractModel

        ( register, registerEffects ) =
            Register.branch.init context
                |> Tea.extractModel
    in
    { header = header
    , home = home
    , login = login
    , register = register
    }
        |> Tea.save
        |> Tea.withChildEffects HeaderMsg applyHeaderEffects headerEffects
        |> Tea.withChildEffects HomeMsg applyHomeEffects homeEffects
        |> Tea.withChildEffects LoginMsg applyLoginEffects loginEffects
        |> Tea.withChildEffects RegisterMsg applyRegisterEffects registerEffects


applyHeaderEffects : Header.Effect -> BaseTea -> BaseTea
applyHeaderEffects _ sprig =
    sprig


applyHomeEffects : Home.Effect -> BaseTea -> BaseTea
applyHomeEffects _ sprig =
    sprig


applyLoginEffects : Login.Effect -> BaseTea -> BaseTea
applyLoginEffects _ sprig =
    sprig


applyRegisterEffects : Register.Effect -> BaseTea -> BaseTea
applyRegisterEffects _ sprig =
    sprig


type alias Model =
    { header : Header.Model
    , home : Home.Model
    , login : Login.Model
    , register : Register.Model
    }


subscriptions : Context -> Model -> Sub Msg
subscriptions context model =
    Sub.batch
        [ Header.branch.subscriptions context model.header
            |> Sub.map HeaderMsg
        , Home.branch.subscriptions context model.home
            |> Sub.map HomeMsg
        , Login.branch.subscriptions context model.login
            |> Sub.map LoginMsg
        , Register.branch.subscriptions context model.register
            |> Sub.map RegisterMsg
        ]


type Msg
    = HeaderMsg Header.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg


type alias Effect =
    Never


update : Context -> Msg -> Model -> BaseTea
update context msg model =
    case msg of
        HeaderMsg headerMsg ->
            Header.branch.update context headerMsg model.header
                |> Tea.mapMsg HeaderMsg
                |> Tea.mapModel (\header -> { model | header = header })
                |> Tea.applyEffects applyHeaderEffects

        HomeMsg homeMsg ->
            Home.branch.update context homeMsg model.home
                |> Tea.mapMsg HomeMsg
                |> Tea.mapModel (\home -> { model | home = home })
                |> Tea.applyEffects applyHomeEffects

        LoginMsg loginMsg ->
            Login.branch.update context loginMsg model.login
                |> Tea.mapMsg LoginMsg
                |> Tea.mapModel (\login -> { model | login = login })
                |> Tea.applyEffects applyLoginEffects

        RegisterMsg registerMsg ->
            Register.branch.update context registerMsg model.register
                |> Tea.mapMsg RegisterMsg
                |> Tea.mapModel (\register -> { model | register = register })
                |> Tea.applyEffects applyRegisterEffects


urlChanged : Context -> Model -> BaseTea
urlChanged context model =
    Header.branch.urlChanged context model.header
        |> Tea.mapMsg HeaderMsg
        |> Tea.mapModel (\header -> { model | header = header })
        |> Tea.applyEffects applyHeaderEffects
        |> Tea.andThen
            (\m ->
                Home.branch.urlChanged context m.home
                    |> Tea.mapMsg HomeMsg
                    |> Tea.mapModel (\home -> { m | home = home })
                    |> Tea.applyEffects applyHomeEffects
            )
        |> Tea.andThen
            (\m ->
                Login.branch.urlChanged context m.login
                    |> Tea.mapMsg LoginMsg
                    |> Tea.mapModel (\login -> { m | login = login })
                    |> Tea.applyEffects applyLoginEffects
            )
        |> Tea.andThen
            (\m ->
                Register.branch.urlChanged context m.register
                    |> Tea.mapMsg RegisterMsg
                    |> Tea.mapModel (\register -> { m | register = register })
                    |> Tea.applyEffects applyRegisterEffects
            )


view : Context -> Model -> Html Msg
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
