module Context exposing
    ( Branch
    , Context
    , Flags
    , MyTea
    , Route
    )

import Api
import Tea exposing (Tea)


type alias Context =
    Tea.Context Flags


type alias Flags =
    Maybe Api.User


type alias Branch model msg effect =
    Tea.Branch Flags model msg effect


type alias Route model msg effect =
    Tea.Route Flags model msg effect


type alias MyTea model msg effect =
    Tea Flags model msg effect
