{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Miso
import Miso.String
import qualified Data.List as List
-- import Data.Char (isUpper)
import qualified Data.Char as Char
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq

#ifndef __GHCJS__
import Data.Function ((&))
import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif

#ifndef __GHCJS__
-- Dev server mode. Runs a HTTP server, serving only the webclient
-- application. Bypasses slow GHCJS compilation by using JSaddle,
-- opting instead to basically run page logic server-side.
runApp :: JSM () -> IO ()
runApp app = do
  let
    port =
      8080
    warpSettings =
      Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "*" -- asterisk => both IPv4 and IPv6
      & Warp.setTimeout 3600
    jsaddle = do
      JSaddle.jsaddleOr
        defaultConnectionOptions
        (app >> syncPoint)
        JSaddle.jsaddleApp
  putStrLn $
    "Running dev server on http://localhost:" ++ show port
  Warp.runSettings warpSettings =<< jsaddle
#else
-- Release mode. Not starting a server. GHCJS instead compiles the
-- application to a JS file.
runApp :: IO () -> IO ()
runApp = id
#endif

data Model =
  Model
  { inputField :: MisoString
  , numField :: MisoString
  , rotNum :: Int
  , rotMap :: Map Char Char
  , rotField :: MisoString
  }
  deriving (Eq, Show)

initModel :: Model
initModel =
  Model
  { inputField = ""
  , numField = ""
  , rotNum = rn
  , rotMap = mkRotMap rn
  , rotField = ""
  }
  where
    rn = 13

data Action
  = NoOp
  | Encrypt MisoString
  | SetRotNum MisoString
  deriving (Show, Eq)

lowerCaseAlpha :: [Char]
lowerCaseAlpha =
  ['a'..'z'] ++ ['æ','ø','å']

upperCaseAlpha :: [Char]
upperCaseAlpha =
  List.map Char.toUpper lowerCaseAlpha

toNum :: Char -> MisoString
toNum c =
  maybe "__" (toStr . succ) (List.findIndex (== c) lowerCaseAlpha)
  where

toStr :: Int -> MisoString
toStr =
  pack . (' ':) . show

substCipher :: Map Char Char -> Char -> Char
substCipher mp c =
  maybe c id (Map.lookup c mp)

mkRotMap :: Int -> Map Char Char
mkRotMap n =
  Map.fromList (lcase ++ ucase)
  where
    lcase =
      List.zip
      lowerCaseAlpha
      (List.drop n (cycle lowerCaseAlpha))
    ucase =
      List.zip
      upperCaseAlpha
      (List.drop n (cycle upperCaseAlpha))

updateModel :: Action -> Model -> Effect Action Model
updateModel action model' =
  case action of
    NoOp ->
      noEff model'
    Encrypt str -> do
      let
        rm =
          rotMap model'
        rf =
          Miso.String.map (substCipher rm) str
        nf =
          Miso.String.concatMap toNum (toLower str)
      noEff (model' { inputField = str, numField = nf, rotField = rf })
    SetRotNum numStr -> do
      let
        rn =
          maybe
          (rotNum model')
          id
          (readMaybe (unpack numStr))
        rm =
          mkRotMap rn
      model' { rotNum = rn, rotMap = rm } <# pure (Encrypt (inputField model'))

viewModel :: Model -> View Action
viewModel model' =
  div_
  []
  [ h1_ [] [text "Tallifisér"]
  , p_ [] [text "Skriv inn teksten din, så koder jeg den om."]
  , textarea_ [onInput Encrypt] []
  , p_ [] [text ("Tallifisering:" <> numField model')]

  , input_ [onInput SetRotNum, maxlength_ "3", value_ (ms (rotNum model'))]
  , p_ [] [text ("ROT-" <> ms (rotNum model') <> ": " <> rotField model')]
  ]

main :: IO ()
main =
  runApp $ startApp $ App
  { initialAction =
      NoOp
  , model =
      initModel
  , update =
      updateModel
  , view =
      viewModel
  , events =
      defaultEvents
  , subs =
      []
  , mountPoint =
      Nothing -- Nothing => 'body'
  }
