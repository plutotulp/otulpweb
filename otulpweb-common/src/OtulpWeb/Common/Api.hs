-- {-# language QuasiQuotes #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module OtulpWeb.Common.Api where

import Servant.API

import Graphics.Svg.Core as Svg
import Data.Text (Text)

-- | Wrapper for 'Svg.Element', to facilitate writing typeclass
-- instances without introducing orphans.
newtype SvgElement = SvgElement Svg.Element

type ApiV1 =
  "show" :> Get '[JSON] Text
  :<|>
  "tournament" :> Capture "n" Int :> Get '[SvgElement] SvgElement

type Api =
  "v1" :> ApiV1

type TopLevelRoutes =
  "metrics" :> Get '[PlainText] Text
  :<|>
  "healthcheck" :> Get '[PlainText] Text
  :<|>
  "api" :> Api
  :<|>
  Raw
