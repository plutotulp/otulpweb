{-# language CPP #-}
{-# language OverloadedStrings #-}

module BaseM
  ( BaseM
  , runApp
  ) where

#ifndef __GHCJS__
import Data.Function ((&))
import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Warp as JSaddle
import Miso (syncPoint)
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif

#ifndef __GHCJS__
type BaseM = JSM
-- Dev server mode. Runs a HTTP server, serving only the webclient
-- application. Bypasses slow GHCJS compilation by using JSaddle,
-- opting instead to basically run page logic server-side.
runApp :: BaseM () -> IO ()
runApp app = do
  let
    port =
      8080
    warpSettings =
      Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "*" -- asterisk => both IPv4 and IPv6
      & Warp.setTimeout 3600
    jsaddle = do
      JSaddle.jsaddleOr
        defaultConnectionOptions
        (app >> syncPoint)
        JSaddle.jsaddleApp
  putStrLn $
    "Running dev server on http://localhost:" ++ show port
  Warp.runSettings warpSettings =<< jsaddle
#else
-- Release mode. Not starting a server. GHCJS instead compiles the
-- application to a JS file.
type BaseM = IO
runApp :: IO () -> IO ()
runApp = id
#endif
