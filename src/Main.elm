module Main exposing (main)

import Api
import Base
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
