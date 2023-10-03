module Main exposing (Effect, Model, Msg(..), main)

import Api
import Base
import Browser
import Browser.Navigation
import Context exposing (Context)
import Html
import Json.Decode
import Json.Encode
import Tea exposing (Tea)
import Url exposing (Url)


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


tree : Tea.Tree Json.Encode.Value Context.Flags Base.Model Base.Msg Base.Effect
tree =
    Tea.tree
        (Json.Decode.decodeValue Api.decodeUser >> Result.toMaybe)
        Base.branch


type alias Model =
    { navKey : Browser.Navigation.Key
    , root : Base.Model
    , context : Context
    }


init : Json.Encode.Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( initialSprig, initialContext ) =
            tree.init flags url
    in
    initialSprig
        |> Tea.mapMsg TreeMsg
        |> Tea.mapModel (\root -> { root = root, navKey = navKey, context = initialContext })
        |> Tea.applyEffects applyCarlEffects
        |> Tea.complete


applyCarlEffects : Base.Effect -> Tea Model Msg Effect -> Tea Model Msg Effect
applyCarlEffects eff sprig =
    case eff of
        Base.Navigate url ->
            sprig
                |> Tea.andThen
                    (\model ->
                        model
                            |> Tea.save
                            |> Tea.withCmd (Browser.Navigation.pushUrl model.navKey (String.join "/" url))
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
                newContext : Context
                newContext =
                    Tea.urlChanged url model.context
            in
            tree.urlChanged newContext model.root
                |> Tea.mapMsg TreeMsg
                |> Tea.mapModel (\root -> { model | root = root, context = newContext })
                |> Tea.applyEffects applyCarlEffects
                |> Tea.complete

        TreeMsg baseMsg ->
            tree.update model.context baseMsg model.root
                |> Tea.mapMsg TreeMsg
                |> Tea.mapModel (\root -> { model | root = root })
                |> Tea.applyEffects applyCarlEffects
                |> Tea.complete


view : Model -> Browser.Document Msg
view model =
    { title = "Hello, World!"
    , body =
        [ tree.view model.context model.root
            |> Html.map TreeMsg
        ]
    }
