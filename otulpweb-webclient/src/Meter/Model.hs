{-# language DeriveGeneric #-}
-- {-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
-- {-# language OverloadedStrings #-}
-- {-# language TypeApplications #-}

module Meter.Model where

import Control.Lens

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Miso (Sub, Transition, scheduleIO)
import Miso.Subscription.History (URI, uriSub, getCurrentURI)

data Model =
  M
  { currentURI :: Maybe URI
  }
  deriving (Eq, Generic, Show)

initModel :: Model
initModel = M Nothing

transition :: Action -> Transition Action Model ()
transition = \case
  Noop ->
    pure ()
  UpdateURI uri ->
    #currentURI .= Just uri
  GetURI ->
    scheduleIO (UpdateURI <$> getCurrentURI)

data Action
  = Noop
  | UpdateURI URI
  | GetURI
  deriving (Eq, Show)

subsRequired :: [Sub Action]
subsRequired =
  [ uriSub UpdateURI
  ]
