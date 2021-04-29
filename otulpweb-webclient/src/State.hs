-- | Wrappers around "miso"'s model update functions, facilitating
-- model update with 'StateT' monad transformer.
module State
  ( noEff
  , singleEff
  , batchEff
  ) where

import Control.Monad.State (State, StateT, execState, runStateT)
import qualified Miso
import Miso (Effect, (<#))

import BaseM (BaseM)

-- | Update model without firing any actions.
noEff :: model -> State model () -> Effect action model
noEff model act =
  Miso.noEff (execState act model)

-- | Update model and fire off a single action.
singleEff ::
  model ->
  StateT model (Effect action) (BaseM action) ->
  Effect action model
singleEff model act =
  batch =<< runStateT act model
  where
    batch (action, model') =
      model' <# action

-- | Update model and fire off multiple actions.
batchEff ::
  model ->
  StateT model (Effect action) [BaseM action] ->
  Effect action model
batchEff model act =
  batch =<< runStateT act model
  where
    batch (actions, model') =
      Miso.batchEff model' actions
