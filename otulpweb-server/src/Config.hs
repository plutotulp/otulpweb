{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Config where

import Control.Lens
import Data.Word

import Data.Generics.Labels ()
import GHC.Generics (Generic)

import ConfigFile
-- import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
-- import Network.Wai (Application)
-- import qualified System.Metrics.Prometheus.Http.Scrape as Scrape

data Config =
  Cfg
  { clientFilePath :: FilePath
  , listenPort :: Word32
  }
  deriving stock (Generic)

mkConfig :: ConfigFile -> IO Config
mkConfig cf = do
  let clientFilePath = cf ^. #clientFilePath
      listenPort = cf ^. #listenPort
  pure $ Cfg {..}
