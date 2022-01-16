{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language ViewPatterns #-}

-- For the orphan instances of SvgElement (from OtulpWeb.Common.Api)
{-# options_ghc -Wno-orphans #-}

import Control.Lens
-- import Control.Monad
import Servant
-- import Servant.API
import System.Exit
import System.IO

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDV4
import Diagrams (renderDia, mkWidth)
import Diagrams.Backend.SVG (SVG(..), Options(SVGOptions))
import GHC.Generics (Generic)
import qualified Graphics.Svg.Core as Svg
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp
import Polysemy (Embed, Sem, Member, Members)
import qualified Polysemy as P
import qualified Polysemy.Embed as P
import Polysemy.Output (Output)
import qualified Polysemy.Output as P
import Polysemy.State (State)
import qualified Polysemy.State as P
import Text.InterpolatedString.Perl6 (qc)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(ssIndices), unsafeToPiece)

import Config
import ConfigCli
import ConfigFile

import OtulpWeb.Common.Api
import Tournament (tournament)

instance Accept SvgElement where
  contentType _ = "image" // "svg+xml" /: ("charset", "utf-8")

instance MimeRender SvgElement SvgElement where
  mimeRender _ (SvgElement val) = Svg.renderBS val

data GenUUID m a where
  GenUUID :: GenUUID m UUID

P.makeSem ''GenUUID

runGenUUIDIO :: Member (Embed IO) r => Sem (GenUUID ': r) a -> Sem r a
runGenUUIDIO =
  P.interpret \GenUUID -> P.embed UUIDV4.nextRandom

data Timestamp m a where
  GetTimestamp :: Timestamp m UTCTime

P.makeSem ''Timestamp

runTimestampIO :: Member (Embed IO) r => Sem (Timestamp ': r) a -> Sem r a
runTimestampIO =
  P.interpret \GetTimestamp -> P.embed Clock.getCurrentTime

newtype TraceId = TraceId UUID
  deriving (Eq, Generic, Ord, Show)

instance ToJSON TraceId

-- FIXME: Would like to see a WithTrace/WithSpan.
data Tracer m a where
  StartTrace :: Text -> Tracer m TraceId
  AddSpan :: TraceId -> Text -> Tracer m ()
  EndTrace :: TraceId -> Tracer m ()

P.makeSem ''Tracer

data TraceSpan =
  TraceSpan
  { name :: !Text
  , ts   :: !UTCTime
  }
  deriving Generic

instance ToJSON TraceSpan

data TraceStart =
  TraceStart
  { name :: !Text
  , ts   :: !UTCTime
  } deriving Generic

data TracerOutput =
  Trace
  { tid :: TraceId
  , ts :: UTCTime
  , name :: Text
  , spans :: Seq TraceSpan
  }
  deriving Generic

instance ToJSON TracerOutput

type TracerStateEntry = (TraceStart, Seq TraceSpan)

-- | Reinterpret Tracer into three effects: a UUID generator for the
-- trace IDs, the State of the tracer, and an Output stream of ended
-- traces.
runTracer ::
  Members [ GenUUID
          , State (Map TraceId TracerStateEntry)
          , Output TracerOutput
          , Timestamp
          ] r
  => Sem (Tracer ': r) a -> Sem r a
runTracer =
  P.interpret \case
  StartTrace name -> do
    tid <- TraceId <$> genUUID
    ts  <- getTimestamp
    P.modify' $ Map.insert @TraceId @TracerStateEntry tid (TraceStart {..}, mempty)
    pure tid
  AddSpan tid name -> do
    ts  <- getTimestamp
    P.modify' $ \(st :: Map TraceId TracerStateEntry) ->
      let combine (_, [newSpan]) (traceStart, oldSpans) =
            (traceStart, newSpan <| oldSpans)
          combine _ _ = error "inconceivable!"
      in Map.insertWith combine tid (undefined, Seq.singleton (TraceSpan {..})) st
  EndTrace tid -> do
    st0 <- P.get @(Map TraceId TracerStateEntry)
    let del _ _ = Nothing
        (mres, st) = Map.updateLookupWithKey del tid st0
        package (traceStart, spans) =
          P.output $
          Trace { tid  = tid
                , name = view #name traceStart
                , ts   = view #ts   traceStart
                , spans = spans
                }
    P.put st
    maybe (pure ()) package mres

data Logger m a where
  LogTxt :: Text -> Logger m ()
  LogJSON :: ToJSON a => a -> Logger m ()

P.makeSem ''Logger

data LogJSONEntry a =
  Entry
  { ts :: UTCTime
  , payload :: a
  }
  deriving Generic

instance ToJSON a => ToJSON (LogJSONEntry a)

loggerToOutput ::
  Members [Timestamp, Output ByteString] r => Sem (Logger ': r) a -> Sem r a
loggerToOutput  =
  P.interpret \case
  LogTxt txt -> do
    ts <- getTimestamp
    P.output (encodeUtf8 (Text.pack (show ts) <> " " <> txt))
  LogJSON payload -> do
    ts <- getTimestamp
    P.output (BSL.toStrict (Aeson.encode Entry {..}))

tracerOutputToLogger :: Member Logger r => Sem (Output TracerOutput ': r) a -> Sem r a
tracerOutputToLogger = P.runOutputSem logJSON

outputToStderr :: Member (Embed IO) r => Sem (Output ByteString ': r) a -> Sem r a
outputToStderr =
  P.runOutputSem (P.embed . BSC.hPutStrLn stderr)

-- | A rather complex mathematical, and possibly statistical,
-- operation, possibly involving large quantities of data from an
-- external source and maybe machine learning, I'm sure. Hence, an
-- effect instead of just a plain function.
data SquareProg m a where
  SquareOp :: Int -> SquareProg m Int

P.makeSem ''SquareProg

-- | An intern came up with this approximation for SquareProg. I'm
-- sure we'll find a more suitable interpretation in time, but for
-- now, we'll just keep using this one.
interpretPlainSquareProg :: Sem (SquareProg : r) a -> Sem r a
interpretPlainSquareProg = P.interpret \(SquareOp n) -> pure (n*n)

squareProg :: Members [Tracer, SquareProg, Logger] r => Int -> Sem r Int
squareProg n = do
  logTxt [qc|Squaring {n}|]
  tid <- startTrace "square"
  res <- squareOp n
  addSpan tid "complex math"
  endTrace tid
  pure res

-- | Serve given directory. This is meant to provide the webclient.
--
-- FIXME: Implement this as a polysemy effect. I have tried and
-- failed, but I'm sure there exists a type tetris solution.
serveDirectory' :: FilePath -> ServerT Raw m
serveDirectory' dir =
  serveDirectoryWith settings
  where
    settings =
      (defaultWebAppSettings dir)
      { ssIndices = [ unsafeToPiece "index.html"] }

healthcheckProg :: Member Logger r => Sem r Text
healthcheckProg = do
  logTxt [qc|Serve healthcheck|]
  pure "Server is healthy"

metricsProg :: Member Logger r => Sem r Text
metricsProg = do
  logTxt [qc|Serve metrics|]
  pure "Hello, metrics!"

showProg :: Member Logger r => Sem r Text
showProg = do
  logTxt [qc|Serve show|]
  pure "Showing EVERYTHING"

-- FIXME: How do we get screen size? I'm guessing we should just take
-- it at as parameter here and let the client worry about formulating
-- the correct call.
tournamentProg ::
  Members [Tracer, Logger, Embed IO] r => Int -> Sem r SvgElement
tournamentProg n = do
  logTxt ([qc|Serve tournament {n}|] :: Text)
  tid <- startTrace [qc|tournament {n}|]
  addSpan tid "render"
  res <-
    P.embed @IO $ pure
    (SvgElement
     (renderDia SVG
      (SVGOptions (mkWidth 1000) Nothing "" [] True)
      (tournament n)))
  endTrace tid
  pure res

type ApiProgEffectsStack =
  [ Tracer
  , State (Map TraceId TracerStateEntry)
  , Output TracerOutput
  , Logger
  , Timestamp
  , Output ByteString
  , GenUUID
  , Embed IO
  , Embed Handler
  ]

runApiProg :: Sem ApiProgEffectsStack a -> Handler a
runApiProg =
  P.runM
  . P.runEmbedded @IO @Handler liftIO
  . runGenUUIDIO
  . outputToStderr
  . runTimestampIO
  . loggerToOutput
  . tracerOutputToLogger
    -- NOTE: Discarding any remaining tracer state here, by using
    -- evalState instead of runState.
  . P.evalState @(Map TraceId TracerStateEntry) mempty
  . runTracer

runLogged ::
  Sem '[Logger, Timestamp, Output ByteString, Embed IO, Embed Handler] a ->
  Handler a
runLogged =
  P.runM
  . P.runEmbedded @IO @Handler liftIO
  . outputToStderr
  . runTimestampIO
  . loggerToOutput

serveMetrics :: Server Metrics
serveMetrics =
  hoistServer metrics runLogged metricsProg

serveHealthcheck :: Server Healthcheck
serveHealthcheck =
  hoistServer healthcheck runLogged healthcheckProg

serveApi :: Server Api
serveApi =
  hoistServer api runApiProg apiProg

apiV1Prog :: Members '[Tracer, Logger, Embed IO] r => ServerT ApiV1 (Sem r)
apiV1Prog =
  showProg :<|> tournamentProg

apiProg :: Members '[Tracer, Logger, Embed IO] r => ServerT Api (Sem r)
apiProg =
  apiV1Prog :<|> unstableApiProg

unstableApiProg :: Members '[Tracer, Logger] r => ServerT UnstableApi (Sem r)
unstableApiProg =
  hoistServer unstableApi interpretPlainSquareProg squareProg

serveTopLevel :: Config -> Server TopLevelRoutes
serveTopLevel cfg =
  serveMetrics :<|>
  serveHealthcheck :<|>
  serveApi :<|>
  serveDirectory' (cfg ^. #clientFilePath)

app :: Config -> Application
app =
  serve topLevelRoutes . serveTopLevel

main :: IO ()
main = do
  cli <- parseCli
  mCfg <- readConfigFile (cli ^. #configFile)
  case mCfg of
    Left err -> do
      Text.hPutStrLn stderr (ppConfigFileError err)
      exitFailure
    Right cf -> do
      cfg <- mkConfig cf
      let port = cfg ^. #listenPort . to fromIntegral
      Text.hPutStrLn stderr [qc|Server Listening on http://0.0.0.0:{port}|]
      run port (app cfg)
