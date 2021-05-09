{-# language LambdaCase #-}

module Meter.Model where

import Miso (Effect)

data Model = M
  deriving (Eq, Show)

initModel :: Model
initModel = M

updateModel :: Model -> Action -> Effect Action Model
updateModel _ = \case
  Noop ->
    pure M

data Action = Noop
  deriving (Eq, Show)
