-- FIXME: Thought to use this to serve up client during development,
-- but was unable to keep it from caching everything to the point of
-- not showing the updated client after recompilations.

{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}

import Servant.API
import Servant
-- import Servant.Server.StaticFiles
-- import Network.Wai
import Network.Wai.Handler.Warp
-- import Data.Proxy
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(ssIndices), unsafeToPiece)

type API =
  "healthcheck" :> Get '[JSON] [String]
  :<|> Raw

api :: Proxy API
api = Proxy

serveDirectory' :: FilePath -> ServerT Raw m
serveDirectory' dir =
  serveDirectoryWith settings
  where
    settings =
      (defaultWebAppSettings dir) { ssIndices = indices }
    indices =
      [ unsafeToPiece "index.html"]

serveHealthcheck :: Handler [String]
serveHealthcheck =
  pure ["Halla", "Balla"]

server :: Server API
server =
  serveHealthcheck :<|> serveDirectory' "static/"

app :: Application
app =
  serve api server

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  run 8080 app
