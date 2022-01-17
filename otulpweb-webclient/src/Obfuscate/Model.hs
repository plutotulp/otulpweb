{-# language DeriveGeneric #-}
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
  , transition

  -- * Actions and their prisms.
  , Action(..)

  -- * Utilities
  , msVigKey

  ) where

import Control.Lens
import Data.Char

import Data.Generics.Labels ()
import Data.Maybe (mapMaybe, fromMaybe)
import GHC.Generics (Generic)
import Miso (Transition, scheduleIO)
import qualified Miso.String
import Miso.String (MisoString, fromMisoString, ms)
import Text.Read (readMaybe)

import OtulpWeb.Common.Rot (RotKey(..))
import qualified OtulpWeb.Common.Rot as Rot
import OtulpWeb.Common.Vigenere (VigKey(..))
import qualified OtulpWeb.Common.Vigenere as Vig

data Model =
  Model
  { inputText :: MisoString
  , plainText :: MisoString

  , numCt :: MisoString
  , numPt :: MisoString

  , rotKey :: RotKey
  , rotCt :: MisoString
  , rotPt :: MisoString

  , vigKeyText :: MisoString
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

  , rotKey = 0
  , rotCt = ""
  , rotPt = ""

  , vigKeyText = ""
  , vigKey = Vig.mkVigKey ""
  , vigCt = ""
  , vigPt = ""
  }

data Action
  = Encrypt
  | SetInput MisoString
  | SetVigenerePassword MisoString
  | SetRotKey MisoString
  deriving (Show, Eq)

transition :: Action -> Transition Action Model ()
transition = \case
  Encrypt -> do
    pt <- use (#plainText . to fromMisoString)

    let nums = mapMaybe Rot.charToInt pt
    #numCt .= ms (unwords (show <$> nums))
    #numPt .= ms (mapMaybe Rot.intToChar nums)

    rk <- use #rotKey
    let rotCt' = Rot.rotEncString rk pt
        rotPt' = Rot.rotDecString rk rotCt'
    #rotCt .= ms rotCt'
    #rotPt .= ms rotPt'

    vk <- use #vigKey
    let vigCt' = Vig.vigEncString vk pt
        vigPt' = Vig.vigDecString vk vigCt'
    #vigCt .= ms vigCt'
    #vigPt .= ms vigPt'

  SetRotKey str -> do
    let
      rk =
        RotKey (fromMaybe 0 (readMaybe (Miso.String.unpack str)))
    #rotKey .= rk
    scheduleIO (pure Encrypt)

  SetVigenerePassword pwd -> do
    #vigKeyText .= pwd
    #vigKey .= Vig.mkVigKey (toLower <$> fromMisoString pwd)
    scheduleIO (pure Encrypt)

  SetInput inStr -> do
    #inputText .= inStr
    #plainText .=
      ms @String (Rot.validText (toLower <$> fromMisoString inStr))
    scheduleIO (pure Encrypt)

msVigKey :: VigKey -> MisoString
msVigKey =
  ms . Vig.ppVigKey
