module Tea exposing
    ( Tea, Branch, Route
    , plant, branch
    , Context, Model, Msg, RouteModel
    , absolutePath, relativePath
    , flags
    , save
    , withCmd, withMsg
    , withEffect
    , mapModel, mapMsg, andThen, applyEffects, urlChanged
    , extractModel, withChildEffects
    , setFlags, navigate
    , Effects
    )

{-|


# Create

@docs Tea, Branch, Route
@docs plant, branch


## Context

@docs Context, Model, Msg, RouteModel
@docs absolutePath, relativePath
@docs flags


# Update

@docs save
@docs withCmd, withMsg
@docs withEffect


## Update Child

@docs mapModel, mapMsg, andThen, applyEffects, urlChanged
@docs extractModel, withChildEffects


# Effects

@docs setFlags, navigate

-}

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Html exposing (Html)
import Task
import Url exposing (Url)


type Effect flags
    = SetFlags flags
    | Navigate String


{-| Converting your Tea app into a `Browser.application`. Also a play on words, like Tea, cause this is experimental
-}
plant :
    { decodeFlags : encodedFlags -> flags
    , root : Branch flags model msg effect
    , rootEffect : effect -> model -> ( model, Cmd msg )
    }
    ->
        { init : encodedFlags -> Url -> Browser.Navigation.Key -> ( Model model flags, Cmd (Msg msg) )
        , subscriptions : Model model flags -> Sub (Msg msg)
        , update : Msg msg -> Model model flags -> ( Model model flags, Cmd (Msg msg) )
        , view : Model model flags -> Browser.Document (Msg msg)
        , onUrlRequest : Browser.UrlRequest -> Msg msg
        , onUrlChange : Url -> Msg msg
        }
plant options =
    { init =
        \encodedFlags url navKey ->
            let
                flags_ : flags
                flags_ =
                    options.decodeFlags encodedFlags

                contextUrl : AppUrl
                contextUrl =
                    AppUrl.fromUrl url

                initialContext : InternalContext flags
                initialContext =
                    { flags = flags_
                    , url = contextUrl
                    , relativePath = contextUrl.path
                    }

                ( model, cmd ) =
                    options.root.init (Context initialContext)
                        |> mapMsg RootMsg
                        |> harvest
            in
            ( Model
                { rootModel = model
                , navKey = navKey
                , context = initialContext
                }
            , cmd
            )
    , subscriptions =
        \(Model model) ->
            options.root.subscriptions (Context model.context) model.rootModel
                |> Sub.map RootMsg
    , update =
        \msg (Model model) ->
            case msg of
                UrlRequested urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( Model model
                            , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                            )

                        Browser.External url ->
                            ( Model model
                            , Browser.Navigation.load url
                            )

                UrlChanged url ->
                    let
                        (Context newContext) =
                            urlChanged url (Context model.context)

                        (Tea brch) =
                            options.root.urlChanged (Context newContext) model.rootModel
                                |> mapMsg RootMsg
                    in
                    List.foldl
                        (\eff ( m, c ) ->
                            options.rootEffect eff m
                                |> Tuple.mapSecond (\c__ -> Cmd.map RootMsg c__ :: c)
                        )
                        ( brch.model, brch.cmds )
                        brch.effects
                        |> Tuple.mapFirst (\root -> Model { model | rootModel = root, context = newContext })
                        |> Tuple.mapSecond Cmd.batch
                        |> applyInternalEffects brch.internalEffects

                RootMsg msg_ ->
                    let
                        (Tea brch) =
                            options.root.update (Context model.context) msg_ model.rootModel
                                |> mapMsg RootMsg
                    in
                    List.foldl
                        (\eff ( m, c ) ->
                            options.rootEffect eff m
                                |> Tuple.mapSecond (\c__ -> Cmd.map RootMsg c__ :: c)
                        )
                        ( brch.model, brch.cmds )
                        brch.effects
                        |> Tuple.mapFirst (\root -> Model { model | rootModel = root })
                        |> Tuple.mapSecond Cmd.batch
                        |> applyInternalEffects brch.internalEffects
    , view =
        \(Model model) ->
            { title = "TODO"
            , body = [ Html.map RootMsg (options.root.view (Context model.context) model.rootModel) ]
            }
    , onUrlRequest = UrlRequested
    , onUrlChange = UrlChanged
    }


applyInternalEffects : List (Effect flags) -> ( Model model flags, Cmd (Msg msg) ) -> ( Model model flags, Cmd (Msg msg) )
applyInternalEffects effects ( Model initialModel, initialCmd ) =
    List.foldl
        (\eff ( model, cmd ) ->
            case eff of
                SetFlags flags_ ->
                    let
                        context : InternalContext flags
                        context =
                            model.context
                    in
                    ( { model | context = { context | flags = flags_ } }, cmd )

                Navigate path ->
                    ( model, Browser.Navigation.pushUrl model.navKey path :: cmd )
        )
        ( initialModel, [] )
        effects
        |> Tuple.mapFirst Model
        |> Tuple.mapSecond ((::) initialCmd >> Cmd.batch)


{-| The root msg of your Tea app
-}
type Msg msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | RootMsg msg


{-| The overall model of your Tea app
-}
type Model model flags
    = Model
        { rootModel : model
        , navKey : Browser.Navigation.Key
        , context : InternalContext flags
        }


{-| A little more than a `Browser.element` but less than a `Browser.application`
-}
type alias Branch flags model msg effect =
    { init : Context flags -> Tea flags model msg effect
    , subscriptions : Context flags -> model -> Sub msg
    , update : Context flags -> msg -> model -> Tea flags model msg effect
    , urlChanged : Context flags -> model -> Tea flags model msg effect
    , view : Context flags -> model -> Html msg
    }


{-| A little more than a `Browser.element` but less than a `Browser.application`, and can handle URLs
-}
type alias Route flags model msg effect =
    Branch flags (Maybe model) msg effect


{-| Helpful alias for working with routes
-}
type alias RouteModel model =
    Maybe model


{-| Create a branch of your Tea app that can handle URLs
-}
branch :
    { path : List String }
    -> Branch flags model msg effect
    -> Route flags model msg effect
branch cfg branch_ =
    { init =
        \ctx ->
            if relativePath ctx == cfg.path then
                branch_.init ctx
                    |> mapModel Just

            else
                save Nothing
    , subscriptions =
        \ctx model ->
            case model of
                Nothing ->
                    Sub.none

                Just m ->
                    branch_.subscriptions ctx m
    , update =
        \ctx msg model ->
            case model of
                Nothing ->
                    save model

                Just m ->
                    branch_.update ctx msg m
                        |> mapModel Just
    , urlChanged =
        \ctx model ->
            if relativePath ctx == cfg.path then
                case model of
                    Nothing ->
                        branch_.init ctx
                            |> mapModel Just

                    Just m ->
                        branch_.urlChanged ctx m
                            |> mapModel Just

            else
                save model
    , view =
        \ctx model ->
            if relativePath ctx == cfg.path then
                case model of
                    Nothing ->
                        Html.text ""

                    Just m ->
                        branch_.view ctx m

            else
                Html.text ""
    }


{-| For storing things like the logged in User, current theme, etc.
-}
type Context flags
    = Context (InternalContext flags)


type alias InternalContext flags =
    { flags : flags
    , url : AppUrl
    , relativePath : List String
    }


{-| Get the flags out of the context
-}
flags : Context flags -> flags
flags (Context context) =
    context.flags


urlChanged : Url -> Context flags -> Context flags
urlChanged url (Context context) =
    let
        contextUrl : AppUrl
        contextUrl =
            AppUrl.fromUrl url
    in
    Context
        { context
            | url = contextUrl
            , relativePath = contextUrl.path
        }


{-| Get the full URL path
-}
absolutePath : Context flags -> List String
absolutePath (Context context) =
    context.url.path


{-| Get the portion of the URL path that is available to this branch,
i.e. hasn't been consumed by parent components
-}
relativePath : Context flags -> List String
relativePath (Context context) =
    context.relativePath


consumePath : List String -> Context flags -> Context flags
consumePath pathToTake (Context context) =
    Context
        { context
            | relativePath =
                let
                    piecesMatch : Bool
                    piecesMatch =
                        List.map2 Tuple.pair
                            pathToTake
                            context.relativePath
                            |> List.all (\( a, b ) -> a == b)

                    toTakeLength : Int
                    toTakeLength =
                        List.length pathToTake
                in
                if piecesMatch && toTakeLength <= List.length context.relativePath then
                    List.drop toTakeLength context.relativePath

                else
                    context.relativePath
        }


{-| Instead of dealing with `( model, Cmd msg )` we get a more robust type
that allows us to deal with other styles of effects.
-}
type Tea flags model msg effect
    = Tea
        { model : model
        , cmds : List (Cmd msg)
        , effects : List effect
        , internalEffects : List (Effect flags)
        }


{-| Save the state of your model
-}
save : model -> Tea flags model msg effect
save model =
    Tea
        { model = model
        , cmds = []
        , effects = []
        , internalEffects = []
        }


{-| Add a Cmd to your Tea
-}
withCmd : Cmd msg -> Tea flags model msg effect -> Tea flags model msg effect
withCmd cmd (Tea update) =
    Tea { update | cmds = cmd :: update.cmds }


{-| Add a Msg to your Tea, to be handled in a future update
-}
withMsg : msg -> Tea flags model msg effect -> Tea flags model msg effect
withMsg msg (Tea update) =
    Tea { update | cmds = msgToCmd msg :: update.cmds }


{-| Add an effect to your Tea, to be handled by the parent component
-}
withEffect : effect -> Tea flags model msg effect -> Tea flags model msg effect
withEffect effect (Tea update) =
    Tea { update | effects = effect :: update.effects }


{-| The pairing helper for `extractModel`
-}
withChildEffects :
    (childMsg -> parentMsg)
    -> (childEffect -> Tea flags model parentMsg parentEffect -> Tea flags model parentMsg parentEffect)
    -> Effects childMsg childEffect
    -> Tea flags model parentMsg parentEffect
    -> Tea flags model parentMsg parentEffect
withChildEffects mapMsgFn applyEffectFn (Effects effs) (Tea update) =
    List.foldl applyEffectFn
        (Tea
            { model = update.model
            , cmds = update.cmds ++ List.map (Cmd.map mapMsgFn) effs.cmds
            , effects = update.effects
            , internalEffects = update.internalEffects
            }
        )
        effs.effects


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    msg
        |> Task.succeed
        |> Task.perform identity


{-| Map over the model of your Tea
-}
mapModel : (model1 -> model2) -> Tea flags model1 msg effect -> Tea flags model2 msg effect
mapModel fn (Tea update) =
    Tea
        { model = fn update.model
        , cmds = update.cmds
        , effects = update.effects
        , internalEffects = update.internalEffects
        }


{-| Map over the msg of your Tea
-}
mapMsg : (msg1 -> msg2) -> Tea flags model msg1 effect -> Tea flags model msg2 effect
mapMsg fn (Tea update) =
    Tea
        { model = update.model
        , cmds = List.map (Cmd.map fn) update.cmds
        , effects = update.effects
        , internalEffects = update.internalEffects
        }


{-| Go from one Tea to another, with access to the model of the first
-}
andThen : (model1 -> Tea flags model2 msg effects) -> Tea flags model1 msg effects -> Tea flags model2 msg effects
andThen fn (Tea update1) =
    let
        (Tea update2) =
            fn update1.model
    in
    Tea
        { model = update2.model
        , cmds = update2.cmds ++ update1.cmds
        , effects = update2.effects
        , internalEffects = update2.internalEffects ++ update1.internalEffects
        }


{-| Convert the effects of a child into something concrete,
or maybe wrap them in your own effect to pass further upwards
-}
applyEffects :
    (childEffect -> Tea flags model msg parentEffect -> Tea flags model msg parentEffect)
    -> Tea flags model msg childEffect
    -> Tea flags model msg parentEffect
applyEffects fn (Tea update) =
    List.foldr
        (\eff (Tea up) ->
            let
                (Tea next) =
                    fn eff (Tea up)
            in
            Tea { next | internalEffects = next.internalEffects ++ up.internalEffects }
        )
        (Tea
            { model = update.model
            , cmds = update.cmds
            , effects = []
            , internalEffects = update.internalEffects
            }
        )
        update.effects


{-| Used to update the flags in your Tea
-}
setFlags : flags -> Tea flags model msg effect -> Tea flags model msg effect
setFlags flags_ (Tea update) =
    Tea { update | internalEffects = SetFlags flags_ :: update.internalEffects }


{-| For manual, non-link based navigation
-}
navigate : String -> Tea flags model msg effect -> Tea flags model msg effect
navigate path (Tea update) =
    Tea { update | internalEffects = Navigate path :: update.internalEffects }


{-| Useful for when you have multiple child components
-}
extractModel : Tea flags model msg effect -> ( model, Effects msg effect )
extractModel (Tea update) =
    ( update.model
    , Effects
        { cmds = update.cmds
        , effects = []
        }
    )


{-| Just a wrapper for effectful things
-}
type Effects msg effect
    = Effects
        { cmds : List (Cmd msg)
        , effects : List effect
        }


harvest : Tea flags model msg effect -> ( model, Cmd msg )
harvest (Tea update) =
    ( update.model, Cmd.batch update.cmds )
