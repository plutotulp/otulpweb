{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language RankNTypes #-}

module Model

  ( -- * Model and its lenses.
    Model(..)
  , AppName(..)
  , msAppName

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)

    -- * Subscriptions
  , subsRequired

  ) where

import Control.Lens
import Control.Monad

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Miso (Effect, Sub, mapSub, Transition, fromTransition, mapAction, scheduleIO)
import Miso.String (MisoString, ms)

import qualified Obfuscate.Model
import qualified Meter.Model
import qualified Pong.Model

data AppName
  = Top
  | Obfuscate
  | Pong
  | Meter
  deriving (Eq, Generic, Show)

msAppName :: AppName -> MisoString
msAppName = ms . \case
  Top -> "topp"
  Obfuscate -> "obfuskér"
  Pong -> "poing"
  Meter -> "meter"

data Model =
  Model
  { obfuscate :: Obfuscate.Model.Model
  , pong :: Pong.Model.Model
  , meter :: Meter.Model.Model
  , selected :: AppName
  }
  deriving (Eq, Generic, Show)

initModel :: Model
initModel =
  Model
  { obfuscate =
      Obfuscate.Model.initModel
  , pong =
      Pong.Model.initModel
  , meter =
      Meter.Model.initModel
  , selected =
      Obfuscate
  }

data Action
  = Noop
  | ShowApp AppName
  | PongAction Pong.Model.Action
  | ObfuscateAction Obfuscate.Model.Action
  | MeterAction Meter.Model.Action
  deriving (Show, Generic, Eq)

transition :: Action -> Transition Action Model ()
transition = \case
  Noop ->
    pure ()

  ShowApp app -> do
    #selected .= app
    when (app == Meter) $ do
      scheduleIO (pure (MeterAction Meter.Model.GetURI))

  ObfuscateAction action ->
    zoomApp #obfuscate ObfuscateAction Obfuscate.Model.transition action

  PongAction action ->
    zoomApp #pong PongAction Pong.Model.transition action

  MeterAction action ->
    zoomApp #meter MeterAction Meter.Model.transition action

  where
    zoomApp l aw t a =
      zoom l (mapAction aw (t a))

updateModel :: Model -> Action -> Effect Action Model
updateModel model action = fromTransition (transition action) model

subsRequired :: [Sub Action]
subsRequired =
  (mapSub PongAction <$> Pong.Model.subsRequired)
  ++
  (mapSub MeterAction <$> Meter.Model.subsRequired)
