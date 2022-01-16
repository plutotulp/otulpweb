-- {-# language QuasiQuotes #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module OtulpWeb.Common.Api where

import Servant.API

import Graphics.Svg.Core as Svg
import Data.Proxy (Proxy(..))
import Data.Text (Text)

-- | Wrapper for 'Svg.Element', to facilitate writing typeclass
-- instances without introducing orphans.
newtype SvgElement = SvgElement Svg.Element

type ShowApi = Get '[JSON] Text

showApi :: Proxy ShowApi
showApi = Proxy

type TournamentApi = Capture "n" Int :> Get '[SvgElement] SvgElement

tournamentApi :: Proxy TournamentApi
tournamentApi = Proxy

type ApiV1 =
  "show" :> ShowApi
  :<|>
  "tournament" :> TournamentApi

apiV1 :: Proxy ApiV1
apiV1 = Proxy

type UnstableApi =
  "square" :> Capture "n" Int :> Get '[JSON] Int

unstableApi :: Proxy UnstableApi
unstableApi = Proxy

type Api =
  "v1" :> ApiV1
  :<|>
  "unstable" :> UnstableApi

api :: Proxy Api
api = Proxy

type Metrics = Get '[PlainText] Text

metrics :: Proxy Metrics
metrics = Proxy

type Healthcheck = Get '[PlainText] Text

healthcheck :: Proxy Healthcheck
healthcheck = Proxy

type TopLevelRoutes =
  "metrics" :> Metrics
  :<|>
  "healthcheck" :> Healthcheck
  :<|>
  "api" :> Api
  :<|>
  Raw

topLevelRoutes :: Proxy TopLevelRoutes
topLevelRoutes = Proxy

-- FIXME: SVG is missing ToSchema instances, so will have to make some
-- up if the idea is to keep the tournmaent bit in as an API. This
-- might be easier said than done. Maybe better to not claim
-- tournament as part of the API. Note also that this here code
-- requires swagger2 and servant-swagger packages.
--
-- apiSwagger :: _
-- apiSwagger = toSwagger (Proxy @Api)
--   & info . title .~ "Otulpweb API"
