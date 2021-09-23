{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

{-# options_ghc -Wno-orphans #-}

import Servant.API
import Servant
import Control.Lens
import System.Exit
import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Diagrams (renderDia, mkWidth)
import Diagrams.Backend.SVG (SVG(..), Options(SVGOptions))
import qualified Graphics.Svg.Core as Svg
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp
import Text.InterpolatedString.Perl6 (qc)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(ssIndices), unsafeToPiece)

import OtulpWeb.Common.Api
import ConfigCli
import ConfigFile
import Tournament (tournament)

instance Accept SvgElement where
  contentType _ = "image" // "svg+xml" /: ("charset", "utf-8")

instance MimeRender SvgElement SvgElement where
  mimeRender _ (SvgElement val) = Svg.renderBS val

serveDirectory' :: FilePath -> ServerT Raw m
serveDirectory' dir =
  serveDirectoryWith settings -- FIXME: Add logging here
  where
    settings =
      (defaultWebAppSettings dir)
      { ssIndices = [ unsafeToPiece "index.html"] }

serveHealthcheck :: Handler Text
serveHealthcheck = do
  liftIO $ Text.hPutStrLn stderr [qc|Serve healthcheck|]
  pure "Server is healthy"

serveApiV1Metric :: ConfigFile -> Handler Text
serveApiV1Metric _cfg = do
  liftIO $ Text.hPutStrLn stderr [qc|Serve metrics|]
  pure "You need to post some metrics"

serveApiV1Show :: ConfigFile -> Handler Text
serveApiV1Show _cfg = do
  liftIO $ Text.hPutStrLn stderr [qc|Serve show|]
  pure "Showing EVERYTHING"

-- FIXME: How do we get screen size? I'm guessing we should just take
-- it at as parameter here and let the app worry about formualting the
-- correct call.
serveApiV1Tournament :: ConfigFile -> Int -> Handler SvgElement
serveApiV1Tournament _cfg n = do
  liftIO $ Text.hPutStrLn stderr [qc|Serve tournament {n}|]
  pure
    (SvgElement
      (renderDia SVG
        (SVGOptions (mkWidth 1000) Nothing "" [] True)
        (tournament n)))

serveApiV1 :: ConfigFile -> Server ApiV1
serveApiV1 cfg =
  serveApiV1Metric cfg :<|> serveApiV1Show cfg :<|> serveApiV1Tournament cfg

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

-- FIXME: Use a proper logger instead of hPutStrLn. Also add an access
-- log? Or perhaps just trust reverse proxy in front to do that stuff.

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
      Text.hPutStrLn stderr [qc|Listening on http://0.0.0.0:{port}|]
      run port (app cfg)
