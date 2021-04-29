{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Obfuscate.Model

  ( -- * Model and its lenses.
    Model(..)

  -- * Create and update model.
  , initModel
  , updateModel

  -- * Actions and their prisms.
  , Action(..)

  ) where

import Control.Lens

import qualified Data.Char as Char
import Data.Generics.Labels ()
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso (Effect)
import qualified Miso.String
import Miso.String (MisoString)
import Text.Read (readMaybe)

import qualified State

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
  { inputField :: MisoString
  , pwdField :: MisoString
  , numField :: MisoString
  , rotNum :: Int
  , rotMap :: Map Char Char
  , rotField :: MisoString
  }
  deriving (Eq, Generic, Show)

initModel :: Model
initModel =
  Model
  { inputField = ""
  , pwdField = "SUPERSECRET"
  , numField = ""
  , rotNum = rn
  , rotMap = mkRotMap rn
  , rotField = ""
  }
  where
    rn = 13

data Action
  = Encrypt
  | SetInput MisoString
  | SetPassword MisoString
  | SetRotNum MisoString
  deriving (Show, Eq)

toNum :: Char -> MisoString
toNum c =
  maybe "__" (toStr . succ) (List.findIndex (== c) lowerCaseAlpha)

toStr :: Int -> MisoString
toStr =
  Miso.String.pack . (' ':) . show

substCipher :: Map Char Char -> Char -> Char
substCipher mp c =
  maybe c id (Map.lookup c mp)

updateModel :: Model -> Action -> Effect Action Model
updateModel model = \case

    Encrypt -> do
      State.noEff model $ do
        let
          rm =
            model ^. #rotMap
          str =
            model ^. #inputField
        #numField .=
          Miso.String.concatMap toNum (Miso.String.toLower str)
        #rotField .=
          Miso.String.map (substCipher rm) str

    SetRotNum numStr -> do
      State.singleEff model $ do
        let
          rn =
            fromMaybe 0 (readMaybe (Miso.String.unpack numStr))
        #rotNum .=
          rn
        #rotMap .=
          mkRotMap rn
        pure (pure Encrypt)

    SetPassword pwdStr -> do
      State.singleEff model $ do
        #pwdField .= pwdStr
        (pure . pure) Encrypt


    SetInput inStr -> do
      State.singleEff model $ do
        #inputField .= inStr
        pure (pure Encrypt)
