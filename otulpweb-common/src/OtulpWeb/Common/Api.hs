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

-- FIXME: SVG is missing ToSchema instances, so will have to make some
-- up if the idea is to keep the tournmaent bit in as an API. This
-- might be easier said than done. Maybe better to not claim
-- tournament as part of the API. Note also that this here code
-- requires swagger2 and servant-swagger packages.
--
-- apiSwagger :: _
-- apiSwagger = toSwagger (Proxy @Api)
--   & info . title .~ "Otulpweb API"
