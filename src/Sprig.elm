module Sprig exposing
    ( Sprig
    , save, complete
    , withCmd, withMsg, withEffect
    , mapModel, mapMsg
    , andThen
    , applyEffects
    , Branch, Context, Tree, absolutePath, extractModel, flags, relativePath, tree, urlChanged, withChildEffects
    )

{-|

@docs Sprig

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
    { init : encodedFlags -> Url -> ( Sprig model msg effect, Context flags )
    , subscriptions : Context flags -> model -> Sub msg
    , update : Context flags -> msg -> model -> Sprig model msg effect
    , urlChanged : Context flags -> model -> Sprig model msg effect
    , view : Context flags -> model -> Html msg
    }


tree : (encodedFlags -> flags) -> Branch flags model msg effect -> Tree encodedFlags flags model msg effect
tree decodeFlags branch =
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
            ( branch.init context, context )
    , subscriptions = branch.subscriptions
    , urlChanged = branch.urlChanged
    , update = branch.update
    , view = branch.view
    }


type alias Branch flags model msg effect =
    { init : Context flags -> Sprig model msg effect
    , subscriptions : Context flags -> model -> Sub msg
    , update : Context flags -> msg -> model -> Sprig model msg effect
    , urlChanged : Context flags -> model -> Sprig model msg effect
    , view : Context flags -> model -> Html msg
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


type Sprig model msg effect
    = Sprig
        { model : model
        , cmds : List (Cmd msg)
        , effects : List effect
        }


save : model -> Sprig model msg effect
save model =
    Sprig { model = model, cmds = [], effects = [] }


withCmd : Cmd msg -> Sprig model msg effect -> Sprig model msg effect
withCmd cmd (Sprig update) =
    Sprig { update | cmds = cmd :: update.cmds }


withMsg : msg -> Sprig model msg effect -> Sprig model msg effect
withMsg msg (Sprig update) =
    Sprig { update | cmds = msgToCmd msg :: update.cmds }


withEffect : effect -> Sprig model msg effect -> Sprig model msg effect
withEffect effect (Sprig update) =
    Sprig { update | effects = effect :: update.effects }


withChildEffects :
    (childMsg -> parentMsg)
    -> (childEffect -> Sprig model parentMsg parentEffect -> Sprig model parentMsg parentEffect)
    -> Effects childMsg childEffect
    -> Sprig model parentMsg parentEffect
    -> Sprig model parentMsg parentEffect
withChildEffects mapMsgFn applyEffectFn (Effects effs) (Sprig update) =
    List.foldl applyEffectFn
        (Sprig
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


mapModel : (model1 -> model2) -> Sprig model1 msg effect -> Sprig model2 msg effect
mapModel fn (Sprig update) =
    Sprig
        { model = fn update.model
        , cmds = update.cmds
        , effects = update.effects
        }


mapMsg : (msg1 -> msg2) -> Sprig model msg1 effect -> Sprig model msg2 effect
mapMsg fn (Sprig update) =
    Sprig
        { model = update.model
        , cmds = List.map (Cmd.map fn) update.cmds
        , effects = update.effects
        }


andThen : (model1 -> Sprig model2 msg effects) -> Sprig model1 msg effects -> Sprig model2 msg effects
andThen fn (Sprig update1) =
    let
        (Sprig update2) =
            fn update1.model
    in
    Sprig
        { model = update2.model
        , cmds = update2.cmds ++ update1.cmds
        , effects = update2.effects
        }


applyEffects :
    (childEffect -> Sprig model msg parentEffect -> Sprig model msg parentEffect)
    -> Sprig model msg childEffect
    -> Sprig model msg parentEffect
applyEffects fn (Sprig update) =
    List.foldr fn
        (Sprig
            { model = update.model
            , cmds = update.cmds
            , effects = []
            }
        )
        update.effects


extractModel : Sprig model msg effect -> ( model, Effects msg effect )
extractModel (Sprig update) =
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


complete : Sprig model msg effect -> ( model, Cmd msg )
complete (Sprig update) =
    ( update.model, Cmd.batch update.cmds )
