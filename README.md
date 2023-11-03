# Earl Grey

An **experimental** Elm package to make building _apps_ easier.

Inspired by projects like [Elm Land](https://elm.land/) and [React Router](https://reactrouter.com), my thought was "what if everything was TEA?". This has led me to the idea of a "component" setup in Elm where each component is a self-contained TEA app **with effects**.

## The gist of it:

```elm
module MyComponent exposing (branch)

import Tea exposing (Tea)


branch : Tea.Branch Flags Model Msg Effect
branch =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Tea.Context Flags -> Tea Flags Model Msg Effect


subscriptions : Tea.Context Flags -> Model -> Sub Msg


update : Tea.Context Flags -> Msg -> Model -> Tea Flags Model Msg Effect


urlChanged : Tea.Context Flags -> Model -> Tea Flags Model Msg Effect


view : Tea.Context Flags -> Model -> Html Msg



type alias Flags = ...

type alias Model = ...

type Msg = ...

type Effect = ...
```

### Things to note about this:

#### Repition

There's a lot of type repition going on here, so I have a module like so

```elm
module Context exposing
    ( Branch
    , Context
    , Flags
    , MyTea
    , Route
    )

import Tea exposing (Tea)


type alias Flags =
    -- Whatever you want
    {}


type alias Context =
    Tea.Context Flags


type alias Branch model msg effect =
    Tea.Branch Flags model msg effect

{-| We haven't talked about this yet,
but it's coming up soon
-}
type alias Route model msg effect =
    Tea.Route Flags model msg effect


type alias MyTea model msg effect =
    Tea Flags model msg effect

```

#### What's `urlChanged`?

**NOTE:** This is likely to change to accomodate things beyond URLs changing. E.g. what if your app has dynamic feature flags and those change? It'd be nice to have anything in the `Context` trigger an update.

This gets called any time the URL has changed **&** your component's `init` has already been called. It's a way for your component to react to the URL changing without having to re-initialize, and without defining an interal hook like:

```elm
module MyComponent epxosing (Msg, urlChanged)

type Msg = UrlChanged Url

urlChanged : Url -> Msg
urlChange = UrlChanged
```

```elm
module Parent epxosing (..)

import MyComponent

update msg model =
    case msg of
        UrlChanged url ->
            MyComponent.update
                (MyComponent.urlChanged url)
                model.myComponent
                |> ...
```

This is hard to remember and track, as well as reptitive to check on every time you want to use a child component.

## Routing

**NOTE:** This portion is the most experimental and likely to change

Routing will look very similar, with a slight modification, a path is specified! Below is an example of the setup for a component that only renders when the URL is `*/login`

```elm
module MyComponent exposing (branch)

import Tea exposing (Tea)


branch : Context.Route InternalModel Msg Effect
branch =
    Tea.branch
        { path = [ "login" ]
        }
        { init = init
        , subscriptions = subscriptions
        , update = update
        , urlChanged = urlChanged
        , view = view
        }

type alias Model =
    Tea.RouteModel InternalModel


init : Tea.Context Flags -> Tea Flags InternalModel Msg Effect


subscriptions : Tea.Context Flags -> InternalModel -> Sub Msg


update : Tea.Context Flags -> Msg -> InternalModel -> Tea Flags InternalModel Msg Effect


urlChanged : Tea.Context Flags -> InternalModel -> Tea Flags InternalModel Msg Effect


view : Tea.Context Flags -> InternalModel -> Html Msg



type alias Flags = ...

type alias InternalModel = ...

type Msg = ...

type Effect = ...
```

You'll also notice that the `Model` is now a `Tea.RouteModel` type alias. This is because a route can handle caching itself and calling either `init` or `urlChanged` depending on cache state.

## Structure

### Main.elm

The current approach is to define an application like so:

```elm
module Main exposing (main)

import Api -- This assumes you have a module to define your API calls
import Base -- This is the initial "branch" of your app
import Browser
import Context
import Json.Decode
import Json.Encode
import Tea


main : Program Json.Encode.Value (Tea.Model Base.Model Context.Flags) (Tea.Msg Base.Msg)
main =
    Tea.plant
        { decodeFlags = Json.Decode.decodeValue Api.decodeUser >> Result.toMaybe
        , root = Base.branch
        , rootEffect = \_ model -> ( model, Cmd.none )
        }
        |> Browser.application

```

I want to support non-applications as well, though I'm still figuring out what that looks like. E.g. if you don't have an application, can you have a `Tea.route`? How would a URL change be handled? And lots more.

### Component with multuple Component children

```elm
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
```
