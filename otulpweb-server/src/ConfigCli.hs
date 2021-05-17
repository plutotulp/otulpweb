-- {-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
-- {-# language LambdaCase #-}
-- {-# language OverloadedLabels #-}
{-# language RankNTypes #-}
{-# language DerivingStrategies #-}

module ConfigCli where

import Options.Applicative

import Data.Generics.Labels ()
import GHC.Generics (Generic)
-- import Data.Text (Text)

data ConfigCli =
  Cfg
  { configFile :: FilePath
  }
  deriving (Generic, Show)

configCliParser :: Parser ConfigCli
configCliParser = Cfg <$> configFileParser

configFileParser :: Parser FilePath
configFileParser =
  strOption
  (    long "config"
       <> short 'c'
       <> metavar "FILE"
       <> value "config.dhall"
       <> help "Configuration file path" )

parseCli :: IO ConfigCli
parseCli = execParser opts
  where
    opts = info (configCliParser <**> helper)
           (    fullDesc
             <> progDesc "wow"
             <> header "server - otulpweb HTTP server")
