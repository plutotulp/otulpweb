{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module View
  ( viewModel
  ) where

import Control.Lens

import Miso hiding (model)
-- import Miso.String
-- import Miso.Svg
-- import qualified Miso.Svg as Svg

import Model
import qualified Obfuscate.View
import qualified Pong.View

viewTop :: Model -> View Action
viewTop _model =
  div_
  [ ]
  [ h1_ [] [text "Velg greie!"]
  , button_ [onClick (ShowApp Obfuscate)] [text "obfuskére!"]
  , button_ [onClick (ShowApp Pong)] [text "poing!"]
  ]

viewModel :: Model -> View Action
viewModel model =
  case model ^. #selected of
    Top ->
      viewTop model
    Obfuscate ->
      ObfuscateAction
      <$> Obfuscate.View.viewModel (model ^. #obfuscate)
    Pong ->
      PongAction
      <$> Pong.View.viewModel (model ^. #pong)
