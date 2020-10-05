{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Pong.Model

  ( -- * Model and its lenses.
    Model(..)
  , ballPos
  , ballVel
  , mouseAt
  , windowSize

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)
  , _WasdArrows
  , _MousePos

  -- * Subscriptions required
  , subsRequired

  ) where

import Control.Lens
import Linear.V2
import Linear.Affine
import Linear.Metric

-- import Control.Monad.State (State, StateT, execState, runStateT)
-- import Control.Monad.State (State, execState)
-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromMaybe)
-- import Miso (Effect, (<#), noEff, KeyCode(KeyCode))
import Miso (Effect, Sub)
-- import qualified Miso.String
-- import Miso.String (MisoString, ms)
-- import Text.Read (readMaybe)
import Miso.Subscription.Keyboard (Arrows(..), arrowsSub, wasdSub)
import Miso.Subscription.Mouse (mouseSub)
import Miso.Subscription.Window (windowCoordsSub)

import qualified State

data Model =
  Model
  { _ballPos :: V2 Double
  , _ballVel :: V2 Double
  , _mouseAt :: V2 Int
  , _windowSize :: V2 Int
  , _bodyMargin :: Int
  }
  deriving (Eq, Show)

makeLenses ''Model

initModel :: Model
initModel =
  Model
  { _ballPos = V2 200 200
  , _ballVel = V2 0 0
  , _mouseAt = V2 0 0
  , _windowSize = V2 800 600
  , _bodyMargin = 8
  }

data Action
  = WasdArrows Arrows
  | ArrowArrows Arrows
  | MousePos (Int, Int)
  | WindowSize (Int, Int)
  | ChaseMouse
  deriving (Show, Eq)

makePrisms ''Action

updateModel :: Model -> Action -> Effect Action Model
updateModel model = \case

  ArrowArrows arrows -> do
    State.singleEff model $ do
      pure (pure (WasdArrows arrows))

  WasdArrows arrows -> do
    State.noEff model $ do
      let
        delta =
          V2
          (fromIntegral (arrowX arrows) * 10)
          (fromIntegral (negate (arrowY arrows)) * 10)
      ballPos %= (+ delta)

  MousePos (x, y) -> do
    State.singleEff model $ do
      let
        mrg =
          model^.bodyMargin
      mouseAt .= V2 (x-mrg) (y-mrg)
      pure (pure ChaseMouse)

  WindowSize (h, w) -> do
    State.noEff model $ do
      windowSize .= V2 w h

  ChaseMouse -> do
    let
      mp =
        fromIntegral <$> model^.mouseAt
      bp =
        model^.ballPos
    case distanceA mp bp < 0.5 of
      False -> do
        let
          dist =
            distanceA mp bp
          diff =
            (* (min dist 5)) <$> normalize (mp .-. bp)
        State.singleEff model $ do
          ballPos %= (+diff)
          pure (pure ChaseMouse)
      True ->
        pure model

subsRequired :: [Sub Action]
subsRequired =
  [ wasdSub WasdArrows
  , arrowsSub ArrowArrows
  , mouseSub MousePos
  , windowCoordsSub WindowSize
  ]
