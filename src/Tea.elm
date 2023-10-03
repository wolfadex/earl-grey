module Tea exposing
    ( Tea
    , save, complete
    , withCmd, withMsg, withEffect
    , mapModel, mapMsg
    , andThen
    , applyEffects
    , Branch, Context, Route, RouteModel, Tree, absolutePath, branch, extractModel, flags, relativePath, tree, urlChanged, withChildEffects
    )

{-|

@docs Tea

@docs save, complete

@docs withCmd, withMsg, withEffect
@docs mapModel, mapMsg
@docs andThen
@docs applyEffects

-}

import AppUrl exposing (AppUrl)
import Html exposing (Html)
import Task
import Url exposing (Url)


type alias Tree encodedFlags flags model msg effect =
    { init : encodedFlags -> Url -> ( Tea model msg effect, Context flags )
    , subscriptions : Context flags -> model -> Sub msg
    , update : Context flags -> msg -> model -> Tea model msg effect
    , urlChanged : Context flags -> model -> Tea model msg effect
    , view : Context flags -> model -> Html msg
    }


tree : (encodedFlags -> flags) -> Branch flags model msg effect -> Tree encodedFlags flags model msg effect
tree decodeFlags branch_ =
    { init =
        \flags_ url ->
            let
                contextUrl =
                    AppUrl.fromUrl url

                context =
                    Context
                        { flags = decodeFlags flags_
                        , url = contextUrl
                        , relativePath = contextUrl.path
                        }
            in
            ( branch_.init context, context )
    , subscriptions = branch_.subscriptions
    , urlChanged = branch_.urlChanged
    , update = branch_.update
    , view = branch_.view
    }


type alias Branch flags model msg effect =
    { init : Context flags -> Tea model msg effect
    , subscriptions : Context flags -> model -> Sub msg
    , update : Context flags -> msg -> model -> Tea model msg effect
    , urlChanged : Context flags -> model -> Tea model msg effect
    , view : Context flags -> model -> Html msg
    }


type alias Route flags model msg effect =
    Branch flags (Maybe model) msg effect


type alias RouteModel model =
    Maybe model


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


type Context flags
    = Context
        { flags : flags
        , url : AppUrl
        , relativePath : List String
        }


flags : Context flags -> flags
flags (Context context) =
    context.flags


urlChanged : Url -> Context flags -> Context flags
urlChanged url (Context context) =
    let
        contextUrl =
            AppUrl.fromUrl url
    in
    Context
        { context
            | url = contextUrl
            , relativePath = contextUrl.path
        }


absolutePath : Context flags -> List String
absolutePath (Context context) =
    context.url.path


relativePath : Context flags -> List String
relativePath (Context context) =
    context.relativePath


consumePath : List String -> Context flags -> Context flags
consumePath pathToTake (Context context) =
    Context
        { context
            | relativePath =
                let
                    piecesMatch =
                        List.map2 Tuple.pair
                            pathToTake
                            context.relativePath
                            |> List.all (\( a, b ) -> a == b)

                    toTakeLength =
                        List.length pathToTake
                in
                if piecesMatch && toTakeLength <= List.length context.relativePath then
                    List.drop toTakeLength context.relativePath

                else
                    context.relativePath
        }


type Tea model msg effect
    = Tea
        { model : model
        , cmds : List (Cmd msg)
        , effects : List effect
        }


save : model -> Tea model msg effect
save model =
    Tea { model = model, cmds = [], effects = [] }


withCmd : Cmd msg -> Tea model msg effect -> Tea model msg effect
withCmd cmd (Tea update) =
    Tea { update | cmds = cmd :: update.cmds }


withMsg : msg -> Tea model msg effect -> Tea model msg effect
withMsg msg (Tea update) =
    Tea { update | cmds = msgToCmd msg :: update.cmds }


withEffect : effect -> Tea model msg effect -> Tea model msg effect
withEffect effect (Tea update) =
    Tea { update | effects = effect :: update.effects }


withChildEffects :
    (childMsg -> parentMsg)
    -> (childEffect -> Tea model parentMsg parentEffect -> Tea model parentMsg parentEffect)
    -> Effects childMsg childEffect
    -> Tea model parentMsg parentEffect
    -> Tea model parentMsg parentEffect
withChildEffects mapMsgFn applyEffectFn (Effects effs) (Tea update) =
    List.foldl applyEffectFn
        (Tea
            { model = update.model
            , cmds = update.cmds ++ List.map (Cmd.map mapMsgFn) effs.cmds
            , effects = update.effects
            }
        )
        effs.effects


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    msg
        |> Task.succeed
        |> Task.perform identity


mapModel : (model1 -> model2) -> Tea model1 msg effect -> Tea model2 msg effect
mapModel fn (Tea update) =
    Tea
        { model = fn update.model
        , cmds = update.cmds
        , effects = update.effects
        }


mapMsg : (msg1 -> msg2) -> Tea model msg1 effect -> Tea model msg2 effect
mapMsg fn (Tea update) =
    Tea
        { model = update.model
        , cmds = List.map (Cmd.map fn) update.cmds
        , effects = update.effects
        }


andThen : (model1 -> Tea model2 msg effects) -> Tea model1 msg effects -> Tea model2 msg effects
andThen fn (Tea update1) =
    let
        (Tea update2) =
            fn update1.model
    in
    Tea
        { model = update2.model
        , cmds = update2.cmds ++ update1.cmds
        , effects = update2.effects
        }


applyEffects :
    (childEffect -> Tea model msg parentEffect -> Tea model msg parentEffect)
    -> Tea model msg childEffect
    -> Tea model msg parentEffect
applyEffects fn (Tea update) =
    List.foldr fn
        (Tea
            { model = update.model
            , cmds = update.cmds
            , effects = []
            }
        )
        update.effects


extractModel : Tea model msg effect -> ( model, Effects msg effect )
extractModel (Tea update) =
    ( update.model
    , Effects
        { cmds = update.cmds
        , effects = []
        }
    )


type Effects msg effect
    = Effects
        { cmds : List (Cmd msg)
        , effects : List effect
        }


complete : Tea model msg effect -> ( model, Cmd msg )
complete (Tea update) =
    ( update.model, Cmd.batch update.cmds )
