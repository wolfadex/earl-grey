module User exposing (Internal, User(..), decode, username)

import Json.Decode


type User
    = User Internal


type alias Internal =
    { username : String
    , token : String
    , avatarUrl : String
    }


decode : Json.Decode.Decoder User
decode =
    Json.Decode.map3
        (\username_ token avatarUrl ->
            User
                { username = username_
                , token = token
                , avatarUrl = avatarUrl
                }
        )
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.field "token" Json.Decode.string)
        (Json.Decode.field "avatarUrl" Json.Decode.string)


username : User -> String
username (User user) =
    user.username
