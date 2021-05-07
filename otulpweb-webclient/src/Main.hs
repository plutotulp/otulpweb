module Main (main) where

import Miso
  ( App(..)
  , LogLevel(..)
  , defaultEvents
  , startApp
  )

import BaseM (runApp)
import Model
  ( Action(ShowApp)
  , AppName(Obfuscate)
  , initModel
  , subsRequired
  , updateModel)
import View (viewModel)

main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      ShowApp Obfuscate
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
