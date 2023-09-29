module Base exposing (Effect(..), Model, Msg(..), branch)

import Header
import Home
import Html exposing (Html)
import Html.Attributes
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
    in
    { header = header
    , home = home
    }
        |> Sprig.save
        |> Sprig.withChildEffects HeaderMsg applyHeaderEffects headerEffects
        |> Sprig.withChildEffects HomeMsg applyHomeEffects homeEffects


applyHeaderEffects : Header.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHeaderEffects _ sprig =
    sprig


applyHomeEffects : Home.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHomeEffects _ sprig =
    sprig


type alias Model =
    { home : Home.Model
    , header : Header.Model
    }


subscriptions : Sprig.Context (Maybe User) -> Model -> Sub Msg
subscriptions context model =
    Sub.batch
        [ Header.branch.subscriptions context model.header
            |> Sub.map HeaderMsg
        , Home.branch.subscriptions context model.home
            |> Sub.map HomeMsg
        ]


type Msg
    = Login
    | HeaderMsg Header.Msg
    | HomeMsg Home.Msg


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
        , Home.branch.view context model.home
            |> Html.map HomeMsg
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
