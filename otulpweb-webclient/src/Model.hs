{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}

module Model

  ( -- * Model and its lenses.
    Model(..)
  , AppName(..)

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)

    -- * Subscriptions
  , subsRequired

  ) where

import Control.Lens

-- import Control.Monad.State (State, execState)
-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromMaybe)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
-- import Miso (Effect, (<#), noEff, KeyCode)
import Miso (Effect, Sub, mapSub)
-- import qualified Miso.String
-- import Miso.String (MisoString)
-- import Text.Read (readMaybe)

-- import BaseM (BaseM)
import qualified Obfuscate.Model
import qualified Pong.Model
import qualified State

data AppName
  = Top
  | Obfuscate
  | Pong
  deriving (Eq, Generic, Show)

data Model =
  Model
  { obfuscate :: Obfuscate.Model.Model
  , pong :: Pong.Model.Model
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
  , selected =
      Top
  }

data Action
  = ShowApp AppName
  | PongAction Pong.Model.Action
  | ObfuscateAction Obfuscate.Model.Action
  deriving (Show, Generic, Eq)

-- | Pass on @subAction@ and part of @model@ to the update function,
-- then lift the resulting @Effect subAction subModel@ to an @Effect
-- action model@, slotting the updated @subModel@ back into our
-- @model@.
--
-- Essentially, "zoom in on a sub-app and update model there".
liftApp ::
  model ->
  subAction ->
  (subAction -> action) ->
  Lens' model subModel ->
  (subModel -> subAction -> Effect subAction subModel) ->
  Effect action model
liftApp model subAction mkAction subModelLens updateSubModel  =
  bimap
  mkAction
  (flip (set subModelLens) model)
  (updateSubModel (model^.subModelLens) subAction)

updateModel :: Model -> Action -> Effect Action Model
updateModel model = \case

  ShowApp app ->
    State.noEff model (#selected .= app)

  ObfuscateAction action ->
    liftApp model action
    ObfuscateAction #obfuscate Obfuscate.Model.updateModel

  PongAction action ->
    liftApp model action
    PongAction #pong Pong.Model.updateModel

subsRequired :: [Sub Action]
subsRequired =
  mapSub PongAction <$> Pong.Model.subsRequired
