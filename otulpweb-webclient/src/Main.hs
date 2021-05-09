module Main (main) where

import Miso
  ( App(..)
  , LogLevel(..)
  , defaultEvents
  , startApp
  )

import BaseM (runApp)
import Model
  ( Action(Noop)
  , initModel
  , subsRequired
  , updateModel)
import View (viewModel)

main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      Noop
  , Miso.model =
      initModel
  , update =
      flip updateModel
  , Miso.view =
      viewModel
  , events =
      defaultEvents
  , subs =
      subsRequired
  , mountPoint =
      Nothing -- Nothing => 'body'
  , logLevel = DebugPrerender
  }
