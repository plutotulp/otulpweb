-- FIXME: Thought to use this to serve up client during development,
-- but was unable to keep it from caching everything to the point of
-- not showing the updated client after recompilations.

{-# language DataKinds #-}
{-# language TypeOperators #-}

import Servant.API
import Servant
import Servant.Server.StaticFiles
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy
import WaiAppStatic.Storage.Filesystem (webAppSettingsWithLookup)
-- import WaiAppStatic.Types (MaxAge(NoMaxAge), )

type API =
  "healthcheck" :> Get '[JSON] [String]
  :<|> Raw

api :: Proxy API
api = Proxy

serveDirectory' dir =
  serveDirectoryWith (webAppSettingsWithLookup dir noCache)
  where
    noCache =
      const (pure Nothing)

serveHealthcheck =
  pure ["Halla", "Balla"]

server =
  serveHealthcheck :<|> serveDirectory' "result/bin/app.jsexe/"

app =
  serve api server

main =
  run 8080 app
