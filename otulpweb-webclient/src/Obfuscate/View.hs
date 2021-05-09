{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
-- {-# language QuasiQuotes #-}

module Obfuscate.View
  ( viewModel
  ) where

import Control.Lens

-- import Data.String.Interpolate (i)
import Miso hiding (model)
import Miso.String
-- import Miso.Svg
-- import qualified Miso.Svg as Svg

import Obfuscate.Model (Model(..), Action(..), RotKey(..), msVigKey)

ct_ :: MisoString -> [View Action]
ct_ str =
  [ text "CT: ", span_ [ class_ "text-danger" ] [ text str ] ]

pt_ :: MisoString -> [View Action]
pt_ str =
  [ text "PT: ", span_ [ class_ "text-muted" ] [ text str ] ]

viewModel :: Model -> View Action
viewModel model =
  div_ [ class_ "container" ]
  [ div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h1_ [] [ text "Krypto" ]
      , h2_ [] [ text "Hemmelig melding"]
      , p_  [ class_ "lead" ] [ text "Skriv din hemmelige melding her" ]
      , textarea_ [ onInput SetInput
                  , value_ $ model ^. #inputText ] [] ] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "Som tall"]
      , p_ [] (ct_ numCt')
      , p_ [] (pt_ numPt') ] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "ROT"]
      , p_ [] [ text "Nøkkel"
              , input_ [ onInput SetRotKey
                       , maxlength_ "9" -- Invalid numbers above this length
                       , value_ (ms rotKey') ] ]
      , p_ [] (ct_ rotCt')
      , p_ [] (pt_ rotPt') ] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "Vigenére"]
      , p_ [] [ text "Nøkkel"
              , input_ [ onInput SetVigenerePassword
                       , value_ vigPwd' ] ]
      , p_ [] (ct_ vigCt')
      , p_ [] (pt_ vigPt') ] ] ]

  where
    rotKey' = model ^. #rotKey . to unRotKey
    rotCt'  = model ^. #rotCt
    rotPt'  = model ^. #rotPt
    numCt'  = model ^. #numCt
    numPt'   =model ^. #numPt . to ms
    vigPwd' = model ^. #vigKey . to msVigKey
    vigCt'  = model ^. #vigCt
    vigPt'  = model ^. #vigPt
