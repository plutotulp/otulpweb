{-# language OverloadedStrings #-}

module View
  ( viewModel
  ) where

import Control.Lens

import Miso hiding (model)
import Miso.String
import Miso.Svg
import qualified Miso.Svg as Svg

import Model

yline :: Int -> View action
yline y =
  line_
  [ x1_ "0"
  , y1_ (ms (show y))
  , x2_ "800"
  , y2_ (ms y)
  , stroke_ "black"
  , strokeWidth_ "1"
  ]
  []

viewModel :: Model -> View Action
viewModel model =
  div_
  []
  (
  [ h1_ [] [text "Tallifisér"]
  , p_ [] [text "Skriv inn teksten din, så koder jeg den om."]
  , textarea_ [onInput Encrypt] []
  , p_ [] [text ("Tallifisering:" <> (model^.numField))]

  , input_ [onInput SetRotNum, maxlength_ "3", value_ (model^.rotNum.to ms)]
  , p_ [] [text ("ROT-" <> ms (_rotNum model) <> ": " <> (model^.rotField))]
  , svg_
    [Svg.height_ "600", Svg.width_ "800"]
    [circle_ [ cx_ "200"
             , cy_ "200"
             , r_ (ms (model^.rotNum))
             , stroke_ "green"
             , strokeWidth_ "4"
             , fill_ "yellow"]
      []
    ]
  ]
    ++
    (yline <$> [0,100..600]))
