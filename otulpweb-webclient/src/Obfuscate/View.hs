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
import qualified Data.Map as Map

import Obfuscate.Model (Model(..), Action(..), RotKey(..), msVigKey)

ct_ :: MisoString -> [View Action]
ct_ str =
  [ text "> ", span_ [ class_ "text-danger", style_ breakAnywhere ] [ text str ] ]
  where
    breakAnywhere = Map.singleton "line-break" "anywhere"

-- pt_ :: MisoString -> [View Action]
-- pt_ str =
--   [ text "PT ", span_ [ class_ "text-muted" ] [ text str ] ]

viewModel :: Model -> View Action
viewModel model =
  div_ [ class_ "container" ]
  [ div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h1_ [] [ text "Krypto" ] ] ]

  ,  div_ [ class_ "row" ]
     [ textarea_ [ class_ "col"
                 , onInput SetInput
                 , rows_ "3"
                 , value_ (model ^. #inputText)
                 , placeholder_ "hemmelig melding" ] [] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "Tall"]
      , p_ [] (ct_ numCt') ] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "ROT"]
      , p_ [] [ input_ [ onInput SetRotKey
                       , maxlength_ "9" -- Invalid numbers above this length
                       , value_ (ms (if rotKey' == 0 then "" else show rotKey'))
                       , placeholder_ "heltall"
                       , type_ "number" ] ]
      , p_ [] (ct_ rotCt') ] ]

  , div_ [ class_ "row" ]
    [ div_ [ class_ "col" ]
      [ h2_ [] [ text "Vigenère"]
      , p_ [] [ input_ [ onInput SetVigenerePassword
                       , value_ (model ^. #vigKeyText)
                       , placeholder_ "passord" ] ]
      , p_ [] [ text "K ", span_ [ class_ "text-muted" ] [ text vigPwd' ] ]
      , p_ [] (ct_ vigCt') ] ] ]

  where
    rotKey' = model ^. #rotKey . to unRotKey
    rotCt'  = model ^. #rotCt
    -- rotPt'  = model ^. #rotPt
    numCt'  = model ^. #numCt
    -- numPt'   =model ^. #numPt . to ms
    vigPwd' = model ^. #vigKey . to msVigKey
    vigCt'  = model ^. #vigCt
    -- vigPt'  = model ^. #vigPt
