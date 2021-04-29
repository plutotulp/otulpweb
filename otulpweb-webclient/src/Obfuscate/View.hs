{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Obfuscate.View
  ( viewModel
  ) where

import Control.Lens

import Miso hiding (model)
import Miso.String
-- import Miso.Svg
-- import qualified Miso.Svg as Svg

import Obfuscate.Model

-- yline :: Int -> View action
-- yline y =
--   line_
--   [ x1_ "0"
--   , y1_ (ms (show y))
--   , x2_ "800"
--   , y2_ (ms y)
--   , stroke_ "black"
--   , strokeWidth_ "1"
--   ]
--   []

viewModel :: Model -> View Action
viewModel model =
  div_
  [ ]
  [ h1_ [] [text "Tallifisér"]
  , p_ [] [text "Skriv inn teksten din, så koder jeg den om."]
  , textarea_ [onInput SetInput] []
  , p_ [] [text ("Tallifisering:" <> (model ^. #numField))]

  , input_ [onInput SetPassword, maxlength_ "1000", value_ (model ^. #pwdField . to ms)]
  , input_ [onInput SetRotNum, maxlength_ "3", value_ (model ^. #rotNum . to ms)]
  , p_ [] [text ("ROT-" <> ms (model ^. #rotNum) <> ": " <> (model ^. #rotField))]
  ]
