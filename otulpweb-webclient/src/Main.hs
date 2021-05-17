module Main (main) where

import Miso
  ( App(..)
  , LogLevel(..)
  , defaultEvents
  , startApp
  , fromTransition
  )

import BaseM (runApp)
import Model
  ( Action(Noop)
  , initModel
  , subsRequired
  , transition)
import View (viewModel)

-- If main is built with GHC, main is a http server that serves up a
-- server-side-rendered version of the webclient, with minimal bits of
-- Javascript executed in the browser.
--
-- If main is built with GHCJS, main is a directory of Javascript
-- files and index.html that runs the webclient in the browser.
--
-- Look at the definition of 'runApp' in the "BaseM" module for
-- details.
main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      Noop
  , Miso.model =
      initModel
  , update =
      fromTransition . transition
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
