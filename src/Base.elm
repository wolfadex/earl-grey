module Base exposing (Effect(..), Model, Msg(..), branch)

import Header
import Html exposing (Html)
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Branch (Maybe User) Model Msg Effect
branch =
    { init = init
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Sprig.Context (Maybe User) -> Sprig Model Msg Effect
init context =
    Header.branch.init context
        |> Sprig.mapMsg HeaderMsg
        |> Sprig.mapModel
            (\header ->
                { user = Nothing
                , header = header
                }
            )
        |> Sprig.applyEffects applyHeaderEffects


applyHeaderEffects : Header.Effect -> Sprig Model Msg Effect -> Sprig Model Msg Effect
applyHeaderEffects _ sprig =
    sprig


type alias Model =
    { user : Maybe User
    , header : Header.Model
    }


type Msg
    = Login
    | HeaderMsg Header.Msg


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
        ]
