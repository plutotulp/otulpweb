{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Obfuscate.Model

  ( -- * Model and its lenses.
    Model(..)

    -- * Re-exports
  , RotKey(..)

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)

  -- * Utilities
  , msVigKey

  ) where

import Data.Char
import Control.Lens

import Data.Generics.Labels ()
-- import qualified Data.List as List
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import GHC.Generics (Generic)
import Miso (Effect)
import qualified Miso.String
import Miso.String (MisoString, fromMisoString, ms)
import Text.Read (readMaybe)

import OtulpWeb.Common.Rot (RotKey(..))
import qualified OtulpWeb.Common.Rot as Rot
import OtulpWeb.Common.Vigenere (VigKey(..))
import qualified OtulpWeb.Common.Vigenere as Vig

import qualified State

data Model =
  Model
  { inputText :: MisoString
  , plainText :: MisoString

  , numCt :: MisoString
  , numPt :: MisoString

  , rotKey :: RotKey
  , rotCt :: MisoString
  , rotPt :: MisoString

  , vigKey :: VigKey
  , vigCt :: MisoString
  , vigPt :: MisoString
  }
  deriving (Eq, Generic, Show)

initModel :: Model
initModel =
  Model
  { inputText = ""
  , plainText = ""

  , numCt = ""
  , numPt = ""

  , rotKey = rk
  , rotCt = ""
  , rotPt = ""

  , vigKey = vk
  , vigCt = ""
  , vigPt = ""
  }
  where
    rk = 13
    vk = Vig.mkVigKey "passord"

data Action
  = Encrypt
  | SetInput MisoString
  | SetVigenerePassword MisoString
  | SetRotKey MisoString
  deriving (Show, Eq)

updateModel :: Model -> Action -> Effect Action Model
updateModel model = \case

    Encrypt -> do
      State.noEff model $ do

        let pt = Rot.validText (model ^. #inputText . to fromMisoString)

        #plainText .= ms pt

        let nums = mapMaybe Rot.charToInt pt
        #numCt .= ms (concatMap show nums)
        #numPt .= ms (mapMaybe Rot.intToChar nums)

        let rk     = model ^. #rotKey
            rotCt' = Rot.rotEncString rk pt
            rotPt' = Rot.rotDecString rk rotCt'
        #rotCt .= ms rotCt'
        #rotPt .= ms rotPt'

        let vk = model ^. #vigKey
            vigCt' = Vig.vigEncString vk pt
            vigPt' = Vig.vigDecString vk vigCt'
        #vigCt .= ms vigCt'
        #vigPt .= ms vigPt'

    SetRotKey str -> do
      State.singleEff model $ do
        let
          rk =
            RotKey (fromMaybe 0 (readMaybe (Miso.String.unpack str)))
        #rotKey .=
          rk
        pure (pure Encrypt)

    SetVigenerePassword (fromMisoString -> str) -> do
      State.singleEff model $ do
        #vigKey .= Vig.mkVigKey (toLower <$> str)
        pure (pure Encrypt)

    SetInput (fromMisoString -> inStr) -> do
      State.singleEff model $ do
        #inputText .= ms @String (toLower <$> inStr)
        pure (pure Encrypt)

msVigKey :: VigKey -> MisoString
msVigKey =
  ms . Vig.ppVigKey
