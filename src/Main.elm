module Main exposing (Effect, Model, Msg(..), main)

import Base
import Browser
import Browser.Navigation
import Html
import Json.Decode
import Json.Encode
import Sprig exposing (Sprig)
import Url exposing (Url)
import User exposing (User)


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


tree : Sprig.Tree Json.Encode.Value (Maybe User) Base.Model Base.Msg Base.Effect
tree =
    Sprig.tree
        (Json.Decode.decodeValue User.decode >> Result.toMaybe)
        Base.branch


type alias Model =
    { navKey : Browser.Navigation.Key
    , root : Base.Model
    , context : Sprig.Context (Maybe User)
    }


init : Json.Encode.Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( initialSprig, initialContext ) =
            tree.init flags url
    in
    initialSprig
        |> Sprig.mapMsg TreeMsg
        |> Sprig.mapModel (\root -> { root = root, navKey = navKey, context = initialContext })
        |> Sprig.applyEffects applyCarlEffects
        |> Sprig.complete


applyCarlEffects : Base.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyCarlEffects eff sprig =
    case eff of
        Base.Navigate url ->
            sprig
                |> Sprig.andThen
                    (\model ->
                        model
                            |> Sprig.save
                            |> Sprig.withCmd (Browser.Navigation.pushUrl model.navKey (String.join "/" url))
                    )


type alias Effect =
    Never


subscriptions : Model -> Sub Msg
subscriptions model =
    tree.subscriptions model.context model.root
        |> Sub.map TreeMsg


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | TreeMsg Base.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            let
                newContext : Sprig.Context (Maybe User)
                newContext =
                    Sprig.urlChanged url model.context
            in
            tree.urlChanged newContext model.root
                |> Sprig.mapMsg TreeMsg
                |> Sprig.mapModel (\root -> { model | root = root, context = newContext })
                |> Sprig.applyEffects applyCarlEffects
                |> Sprig.complete

        TreeMsg baseMsg ->
            tree.update model.context baseMsg model.root
                |> Sprig.mapMsg TreeMsg
                |> Sprig.mapModel (\root -> { model | root = root })
                |> Sprig.applyEffects applyCarlEffects
                |> Sprig.complete


view : Model -> Browser.Document Msg
view model =
    { title = "Hello, World!"
    , body =
        [ tree.view model.context model.root
            |> Html.map TreeMsg
        ]
    }
