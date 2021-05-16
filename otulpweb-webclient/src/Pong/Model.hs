{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}

module Pong.Model

  ( -- * Model and its lenses.
    Model(..)

  -- * Create and update model.
  , initModel
  , transition
  , updateModel

  -- * Actions and their prisms.
  , Action(..)

  -- * Subscriptions required
  , subsRequired

  ) where

import Control.Lens
import Linear.Affine
import Linear.Metric
import Linear.V2

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Miso (Effect, Sub, Transition, fromTransition, scheduleIO)
import Miso.Subscription.Keyboard (Arrows(..), arrowsSub, wasdSub)
import Miso.Subscription.Mouse (mouseSub)
import Miso.Subscription.Window (windowCoordsSub)

data Model =
  Model
  { ballPos :: V2 Double
  , ballVel :: V2 Double
  , mouseAt :: V2 Int
  , windowSize :: V2 Int
  , bodyMargin :: Int
  }
  deriving (Eq, Generic, Show)

initModel :: Model
initModel =
  Model
  { ballPos = V2 200 200
  , ballVel = V2 0 0
  , mouseAt = V2 0 0
  , windowSize = V2 800 600
  , bodyMargin = 8
  }

data Action
  = WasdArrows Arrows
  | ArrowArrows Arrows
  | MousePos (Int, Int)
  | WindowSize (Int, Int)
  | ChaseMouse
  deriving (Show, Generic, Eq)

transition :: Action -> Transition Action Model ()
transition = \case
  ArrowArrows arrows -> do
    scheduleIO (pure (WasdArrows arrows))

  WasdArrows arrows -> do
    let
      delta =
        V2
        (fromIntegral (arrowX arrows) * 10)
        (fromIntegral (negate (arrowY arrows)) * 10)
    #ballPos %= (+ delta)

  MousePos (x, y) -> do
    mrg <- use #bodyMargin
    #mouseAt .= V2 (x-mrg) (y-mrg)
    scheduleIO (pure ChaseMouse)

  WindowSize (h, w) -> do
    #windowSize .= V2 w h

  ChaseMouse -> do
    mp <- use (#mouseAt . to (fmap fromIntegral))
    bp <- use #ballPos
    let
      doNothing =
        pure ()
      moveCloser = do
        let
          dist =
            distanceA mp bp
          diff =
            (* min dist 5) <$> normalize (mp .-. bp)
        #ballPos %= (+diff)
        scheduleIO (pure ChaseMouse)

    if distanceA mp bp < 0.5 then doNothing else moveCloser

updateModel :: Model -> Action -> Effect Action Model
updateModel m a = fromTransition (transition a) m

subsRequired :: [Sub Action]
subsRequired =
  [ wasdSub WasdArrows
  , arrowsSub ArrowArrows
  , mouseSub MousePos
  , windowCoordsSub WindowSize
  ]
