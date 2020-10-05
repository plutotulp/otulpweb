{-# language OverloadedStrings #-}

module Main (main) where

import Miso
  ( App(..)
  , defaultEvents
  , startApp
  )

import BaseM (runApp)
import Model
  ( Action(ShowApp)
  , AppName(Top)
  , initModel
  , subsRequired
  , updateModel)
import View (viewModel)

main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      ShowApp Top
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
  }
