port module Port exposing (sseConnect, sseDisconnect, sseReceived, storeToken, getStoredToken)

{-| Ports for JavaScript interop: SSE and localStorage.
-}


{-| Connect to an SSE endpoint.
-}
port sseConnect : String -> Cmd msg


{-| Disconnect from the current SSE stream.
-}
port sseDisconnect : () -> Cmd msg


{-| Receive SSE events as JSON strings from JS.
-}
port sseReceived : (String -> msg) -> Sub msg


{-| Store the JWT token in localStorage.
-}
port storeToken : String -> Cmd msg


{-| Receive the stored token on startup.
-}
port getStoredToken : (Maybe String -> msg) -> Sub msg
