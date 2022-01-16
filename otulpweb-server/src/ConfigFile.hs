{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module ConfigFile where

import Data.Word ( Word32 )
import System.IO.Error ( isDoesNotExistError )

import Control.Monad.Catch (handleIf)
import qualified Dhall
import Dhall (FromDhall)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Data.Text (Text)
import Text.InterpolatedString.Perl6 (qc)

data ConfigFile =
  ConfigFile
  { clientFilePath :: FilePath
  , listenPort :: Word32
  }
  deriving stock (Generic, Show)

instance FromDhall ConfigFile

newtype ConfigFileError
  = FileDoesNotExist FilePath
  deriving Show

ppConfigFileError :: ConfigFileError -> Text
ppConfigFileError = \case
  FileDoesNotExist fp ->
    [qc|Configuration file "{fp}" does not exist|]

readConfigFile :: FilePath -> IO (Either ConfigFileError ConfigFile)
readConfigFile fp =
  handleIf isDoesNotExistError dnee tryParseFile
  where
    tryParseFile =
      Right <$> Dhall.detailed (Dhall.inputFile Dhall.auto fp)
    dnee =
      (const . pure . Left . FileDoesNotExist) fp
