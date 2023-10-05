port module Port exposing (storeUser)

import Api
import Json.Encode


port fromElm : ( String, Json.Encode.Value ) -> Cmd msg


storeUser : Api.User -> Cmd msg
storeUser user =
    fromElm ( "STORE_USER", Api.encodeUser user )
