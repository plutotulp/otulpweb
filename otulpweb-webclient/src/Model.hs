{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Model
  ( -- * Model and its lenses.
    Model(..)
  , inputField
  , numField
  , rotNum
  , rotMap
  , rotField

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)
  , _Encrypt
  , _SetRotNum

  ) where

import Control.Lens

import Control.Monad.State (State, StateT, execState, runStateT)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Miso (Effect, (<#), noEff)
import qualified Miso.String
import Miso.String (MisoString)
import Text.Read (readMaybe)

import BaseM (BaseM)

mkRotMap :: Int -> Map Char Char
mkRotMap n =
  Map.fromList (lcase ++ ucase)
  where
    n' =
      -- handles negative n well
      n `mod` List.length lowerCaseAlpha

    lcase =
      List.zip
      lowerCaseAlpha
      (List.drop n' (cycle lowerCaseAlpha))
    ucase =
      List.zip
      upperCaseAlpha
      (List.drop n' (cycle upperCaseAlpha))

lowerCaseAlpha :: [Char]
lowerCaseAlpha =
  ['a'..'z'] ++ ['æ','ø','å']

upperCaseAlpha :: [Char]
upperCaseAlpha =
  List.map Char.toUpper lowerCaseAlpha

data Model =
  Model
  { _inputField :: MisoString
  , _numField :: MisoString
  , _rotNum :: Int
  , _rotMap :: Map Char Char
  , _rotField :: MisoString
  }
  deriving (Eq, Show)

makeLenses ''Model

initModel :: Model
initModel =
  Model
  { _inputField = ""
  , _numField = ""
  , _rotNum = rn
  , _rotMap = mkRotMap rn
  , _rotField = ""
  }
  where
    rn = 13

data Action
  = Encrypt MisoString
  | SetRotNum MisoString
  deriving (Show, Eq)

makePrisms ''Action

toNum :: Char -> MisoString
toNum c =
  maybe "__" (toStr . succ) (List.findIndex (== c) lowerCaseAlpha)

toStr :: Int -> MisoString
toStr =
  Miso.String.pack . (' ':) . show

substCipher :: Map Char Char -> Char -> Char
substCipher mp c =
  maybe c id (Map.lookup c mp)

noEff' :: model -> State model a -> Effect action model
noEff' model act =
  noEff (execState act model)

singleEff ::
  model ->
  StateT model (Effect action) (BaseM action) ->
  Effect action model
singleEff model act =
  batch =<< runStateT act model
  where
    batch (action, model') =
      model' <# action

-- batchEff' ::
--   model ->
--   StateT model (Effect action) [BaseM action] ->
--   Effect action model
-- batchEff' model act =
--   batch =<< runStateT act model
--   where
--     batch (actions, model') =
--       batchEff model' actions

updateModel :: Model -> Action -> Effect Action Model
updateModel model = \case

    Encrypt str -> do
      noEff' model $ do
        let
          rm =
            model^.rotMap
        inputField .=
          str
        numField .=
          Miso.String.concatMap toNum (Miso.String.toLower str)
        rotField .=
          Miso.String.map (substCipher rm) str

    SetRotNum numStr -> do
      singleEff model $ do
        let
          rn =
            fromMaybe 0 (readMaybe (Miso.String.unpack numStr))
        rotNum .=
          rn
        rotMap .=
          mkRotMap rn
        pure (pure (Encrypt (model^.inputField)))
