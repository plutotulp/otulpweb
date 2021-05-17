{-# language QuasiQuotes #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language OverloadedLabels #-}

import Servant.API
import Servant
import Control.Lens
import System.Exit
import System.IO

-- import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
-- import Servant.Server.StaticFiles
-- import Network.Wai
import Network.Wai.Handler.Warp
-- import Data.Proxy
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(ssIndices), unsafeToPiece)
import Text.InterpolatedString.Perl6 (qc)

import OtulpWeb.Common.Api
import ConfigCli
import ConfigFile

serveDirectory' :: FilePath -> ServerT Raw m
serveDirectory' dir =
  serveDirectoryWith settings
  where
    settings =
      (defaultWebAppSettings dir)
      { ssIndices = [ unsafeToPiece "index.html"] }

serveHealthcheck :: Handler Text
serveHealthcheck =
  pure "Server is healthy"

serveApiV1Metric :: ConfigFile -> Handler Text
serveApiV1Metric _cfg =
  pure "You need to post some metrics"

serveApiV1Show :: ConfigFile -> Handler Text
serveApiV1Show _cfg =
  pure "Showing EVERYTHING"

serveApiV1 :: ConfigFile -> Server ApiV1
serveApiV1 cfg =
  serveApiV1Metric cfg :<|> serveApiV1Show cfg

serveApi :: ConfigFile -> Server Api
serveApi =
  serveApiV1

server :: ConfigFile -> Server TopLevelRoutes
server cfg =
  serveHealthcheck :<|>
  serveApi cfg :<|>
  serveDirectory' (cfg ^. #clientFilePath)

app :: ConfigFile -> Application
app cfg =
  serve @TopLevelRoutes Proxy (server cfg)

-- FIXME: Use a proper logger instead of hPutStrLn

main :: IO ()
main = do
  cli <- parseCli
  mCfg <- readConfigFile (cli ^. #configFile)
  case mCfg of
    Left err -> do
      Text.hPutStrLn stderr (ppConfigError err)
      exitFailure
    Right cfg -> do
      let port = cfg ^. #listenPort . to fromIntegral
      Text.hPutStrLn stderr [qc|Running on http://0.0.0.0:{port}|]
      run port (app cfg)
