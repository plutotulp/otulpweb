-- {-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
-- {-# language QuasiQuotes #-}

module Meter.View
  ( viewModel
  ) where

-- import Control.Lens

-- import Data.String.Interpolate (i)
import Miso hiding (model)
-- import Miso.String
-- import Miso.Svg
-- import qualified Miso.Svg as Svg

import Meter.Model (Model(..), Action(..))

-- ct_ :: MisoString -> [View Action]
-- ct_ str =
--   [ text "CT: ", span_ [ class_ "text-danger" ] [ text str ] ]

-- pt_ :: MisoString -> [View Action]
-- pt_ str =
--   [ text "PT: ", span_ [ class_ "text-muted" ] [ text str ] ]

viewModel :: Model -> View Action
viewModel _ =
  div_ [ class_ "container" ]
  [ div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h1_ [] [ text "Meter" ] ] ] ]
