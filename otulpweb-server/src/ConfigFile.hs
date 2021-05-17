{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
-- {-# language OverloadedLabels #-}
{-# language RankNTypes #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}

module ConfigFile where

import Data.Word
import System.IO.Error
-- import Data.Typeable

import Control.Monad.Catch (handleIf)
import qualified Dhall
import Dhall (FromDhall)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.Text (Text)
-- import qualified Data.Text as Text
-- import Data.Text.Encoding (decodeUtf8)
-- import qualified Data.ByteString as BS
import Text.InterpolatedString.Perl6 (qc)

data ConfigFile =
  Cfg
  { clientFilePath :: FilePath
  , listenPort :: Word32
  }
  deriving stock (Generic, Show)

instance FromDhall ConfigFile

data ConfigError
  = FileDoesNotExist FilePath
  deriving Show

ppConfigError :: ConfigError -> Text
ppConfigError = \case
  FileDoesNotExist fp ->
    [qc|Configuration file "{fp}" does not exist|]

readConfigFile :: FilePath -> IO (Either ConfigError ConfigFile)
readConfigFile fp =
  handleIf isDoesNotExistError dnee tryParseFile
  where
    tryParseFile =
      Right <$> Dhall.detailed (Dhall.inputFile Dhall.auto fp)
    dnee =
      (const . pure . Left . FileDoesNotExist) fp
