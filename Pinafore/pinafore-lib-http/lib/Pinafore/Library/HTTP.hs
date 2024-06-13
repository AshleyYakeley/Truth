{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.HTTP
    ( httpLibrary
    ) where

import Changes.Core
import Data.IORef
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Internal as W
import Pinafore.API
import Shapes

data LangServer
    = ApplicationLangServer W.Application
    | LifecycleLangServer (W.Request -> Lifecycle W.Response)

langServerApplication :: LangServer -> W.Application
langServerApplication (ApplicationLangServer appl) = appl
langServerApplication (LifecycleLangServer rlr) = \req -> withLifecycle (rlr req)

langServerLifecycle :: LangServer -> W.Request -> Lifecycle W.Response
langServerLifecycle (LifecycleLangServer rlr) = rlr
langServerLifecycle (ApplicationLangServer appl) =
    \req ->
        lifecycleWith $ \call -> do
            ref <- newIORef $ error "unhandled HTTP request"
            W.ResponseReceived <-
                appl req $ \resp -> do
                    a <- call resp
                    writeIORef ref a
                    return W.ResponseReceived
            readIORef ref

actionToServer :: (W.Request -> Action W.Response) -> LangServer
actionToServer f =
    LifecycleLangServer $ \req -> do
        kr <- runView $ unliftAction $ f req
        return $
            case kr of
                Known r -> r
                Unknown -> W.responseBuilder W.status500 mempty mempty

serverToAction :: LangServer -> (W.Request -> Action W.Response)
serverToAction server req = actionLiftView $ viewLiftLifecycle $ langServerLifecycle server req

-- LangServer
instance HasQGroundType '[] LangServer where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangServer)|]) "Server.HTTP"

-- Request
instance HasQGroundType '[] W.Request where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily W.Request)|]) "Request.HTTP"

-- Response
instance HasQGroundType '[] W.Response where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily W.Response)|]) "Response.HTTP"

-- Settings
instance HasQGroundType '[] W.Settings where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily W.Settings)|]) "Settings.Server.HTTP"

runServer :: W.Settings -> LangServer -> IO ()
runServer settings server = W.runSettings settings $ langServerApplication server

httpLibrary :: [LibraryModule ()]
httpLibrary =
    pure $
    MkLibraryModule "http" $
    namespaceBDS
        "HTTP"
        [ typeBDS "Request" "An HTTP request." (MkSomeGroundType (qGroundType @_ @W.Request)) []
        , typeBDS "Response" "An HTTP response." (MkSomeGroundType (qGroundType @_ @W.Response)) []
        , typeBDS
              "Server"
              "An HTTP server."
              (MkSomeGroundType (qGroundType @_ @LangServer))
              [valPatBDS "Mk" "" actionToServer $ PureFunction $ \s -> (serverToAction s, ())]
        , namespaceBDS
              "Server"
              [ typeBDS
                    "Settings"
                    "Settings for running an HTTP server."
                    (MkSomeGroundType (qGroundType @_ @W.Settings))
                    []
              , valBDS "run" "" runServer
              ]
        ]
