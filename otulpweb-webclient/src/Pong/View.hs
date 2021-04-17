{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Pong.View
  ( viewModel
  ) where

import Control.Lens
import Linear.V2

import qualified Data.Map as Map
import Miso hiding (model)
import Miso.String
import Miso.Svg
import qualified Miso.Svg as Svg

import Pong.Model

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

ball :: Double -> V2 Double -> View action
ball r (V2 x y) =
  circle_
  [ cx_ (ms x)
  , cy_ (ms y)
  , r_ (ms r)
  , stroke_ "green"
  , strokeWidth_ "4"
  , fill_ "yellow"]
  []

viewModel :: Model -> View Action
viewModel model =
  div_
  [ Miso.style_ (Map.fromList [("margin", "0px"), ("padding", "0px"), ("border", "0px")]) ]
  [ Svg.svg_
    [ Svg.height_ (ms (model ^. #windowSize . _y)), Svg.width_ (ms (model ^. #windowSize . _x)) ]
    [ ball 50 (model ^. #ballPos)
    , ball 2 (model ^. #ballPos)
    , text_ [x_ "200", y_ "200"] [text ("Mouse at " <> ms (model ^. #mouseAt . to show))]
    ]
  ]
