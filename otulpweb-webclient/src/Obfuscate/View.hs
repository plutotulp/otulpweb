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

viewModel :: Model -> View Action
viewModel model =
  div_
  [ ]
  [ h1_ [] [ text "Obfuskér" ]

  , h2_ [] [ text "Hemmelig melding"]
  , p_  [] [ text "Skriv din hemmelige melding her" ]
  , textarea_ [ onInput SetInput
              , value_ $ model ^. #inputText
              ]
    []

  , h2_ [] [ text "Som tall"]
  , p_ [] [ text $ "CT: " <> numCt' ]
  , p_ [] [ text $ "PT: " <> numPt' ]

  , h2_ [] [ text "ROT-kodet"]
  , p_ [] [ text "Nøkkel"
          , input_ [ onInput SetRotKey
                   , maxlength_ "9" -- Invalid numbers above this length
                   , value_ (ms rotKey')
                   ]
          ]
  , p_ [] [ text $ "CT: " <> rotCt' ]
  , p_ [] [ text $ "PT: " <> rotPt' ]

  , h2_ [] [ text "Vigenére"]
  , p_ [] [ text "Nøkkel"
          , input_ [ onInput SetVigenerePassword
                   , value_ vigPwd'
                   ]
          ]
  , p_ [] [text $ "CT: " <> vigCt' ]
  , p_ [] [text $ "PT: " <> vigPt' ]

  ]
  where
    rotKey' = model ^. #rotKey . to unRotKey
    rotCt'  = model ^. #rotCt
    rotPt'  = model ^. #rotPt
    numCt'  = model ^. #numCt
    numPt'   =model ^. #numPt . to ms
    vigPwd' = model ^. #vigKey . to msVigKey
    vigCt'  = model ^. #vigCt
    vigPt'  = model ^. #vigPt
