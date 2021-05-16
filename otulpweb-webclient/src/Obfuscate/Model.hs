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
  , transition
  , updateModel

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
import Miso (Effect, Transition, fromTransition, scheduleIO)
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
    rk = 0
    vk = Vig.mkVigKey ""

data Action
  = Encrypt
  | SetInput MisoString
  | SetVigenerePassword MisoString
  | SetRotKey MisoString
  deriving (Show, Eq)

transition :: Action -> Transition Action Model ()
transition = \case
  Encrypt -> do
    pt <- use (#inputText . to fromMisoString)

    #plainText .= ms pt

    let nums = mapMaybe Rot.charToInt pt
    #numCt .= ms (concatMap show nums)
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

  SetVigenerePassword (fromMisoString -> str) -> do
    #vigKey .= Vig.mkVigKey (toLower <$> str)
    scheduleIO (pure Encrypt)

  SetInput (fromMisoString -> inStr) -> do
    #inputText .= ms @String (Rot.validText (toLower <$> inStr))
    scheduleIO (pure Encrypt)

updateModel :: Model -> Action -> Effect Action Model
updateModel m a = fromTransition (transition a) m

msVigKey :: VigKey -> MisoString
msVigKey =
  ms . Vig.ppVigKey
