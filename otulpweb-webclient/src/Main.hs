{-# language OverloadedStrings #-}

module Main (main) where

import Miso
  ( App(..)
  , defaultEvents
  , startApp
  )

import BaseM (runApp)
import Model
import View (viewModel)

main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      Encrypt ""
  , Miso.model =
      initModel
  , update =
      flip updateModel
  , Miso.view =
      viewModel
  , events =
      defaultEvents
  , subs =
      []
  , mountPoint =
      Nothing -- Nothing => 'body'
  }
