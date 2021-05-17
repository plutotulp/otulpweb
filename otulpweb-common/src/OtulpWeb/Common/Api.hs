-- {-# language QuasiQuotes #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module OtulpWeb.Common.Api where

import Servant.API

import Data.Text (Text)

type ApiV1 =
  "metric" :> Get '[PlainText] Text
  :<|>
  "show" :> Get '[JSON] Text

type Api =
  "v1" :> ApiV1

type TopLevelRoutes =
  "healthcheck" :> Get '[PlainText] Text
  :<|>
  "api" :> Api
  :<|>
  Raw
