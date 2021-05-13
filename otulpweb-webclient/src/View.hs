{-# language CPP #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module View
  ( viewModel
  ) where

import Control.Lens
-- import Data.Char

import Miso hiding (model)
import Miso.String

import Model (AppName(..), Action(..), Model(..))

import qualified Meter.View
import qualified Obfuscate.View
import qualified Pong.View

viewTop :: Model -> View Action
viewTop _model =
  div_ [ class_ "container" ]
  [ div_ [ class_ "d-grid gap-2" ]
    [ a_ [ class_ "btn btn-primary"
         , onClick (ShowApp Obfuscate) ]
      [ text "obfuskér" ]
    , a_ [ class_ "btn btn-primary"
         , onClick (ShowApp Pong) ]
      [ text "poing" ]
    , a_ [ class_ "btn btn-primary"
         , onClick (ShowApp Meter) ]
      [ text "meter" ]
    ] ]

breadcrumbs :: AppName -> View Action
breadcrumbs selectedAppName =
  nav_ [ prop "aria-label" ("breadcrumb" :: String) ]
  [ ol_ [ class_ "breadcrumb" ] (mkLi <$> cs) ]
  where
    cs = if selectedAppName == Top
         then [Top]
         else [Top, selectedAppName]

    mkLi name = if name == selectedAppName
                then chosen' name
                else normal name

    chosen' name =
      li_ [ class_ "breadcrumb-item active"
          , prop "aria-current" ("page" :: String)
          , onClick (ShowApp name) ]
      [ (text . toLower . ms . show) name ]

    normal name =
      li_ [ class_ "breadcrumb-item"
          , onClick (ShowApp name)]
      [ (text . toLower . ms . show) name ]

-- Links to external bootstrap CSS and JS. When building for release
-- with GHCJS, this is just an empty list, and we instead rely on
-- bundling these files and refering to them in index.html. During
-- development, and building with plain GHC, this is a of external
-- links bringing in Bootstrap CSS and JS, because with these builds
-- we simply don't have control of index.html, and cannot put these
-- links there.
bootstrapLinks :: [View Action]
bootstrapLinks =
#ifndef __GHCJS__
  [ link_ [ href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
          , rel_ "stylesheet" ]
  , script_ [ src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/js/bootstrap.bundle.min.js" ] "" ]
#else
  []
#endif

viewModel :: Model -> View Action
viewModel model =
  div_ [ class_ "container" ]
  $ bootstrapLinks ++
  [ breadcrumbs (model ^. #selected)
  , case model ^. #selected of
      Top ->
        viewTop model
      Obfuscate ->
        ObfuscateAction
        <$> Obfuscate.View.viewModel (model ^. #obfuscate)
      Pong ->
        PongAction
        <$> Pong.View.viewModel (model ^. #pong)
      Meter ->
        MeterAction
        <$> Meter.View.viewModel (model ^. #meter) ]
