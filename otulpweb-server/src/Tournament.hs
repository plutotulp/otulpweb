{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Tournament where

import Diagrams.Prelude
import Diagrams.Backend.SVG

tournament :: Int -> Diagram B
tournament k
  | k  < 1    = tooFewNodes
  | 20 < k    = tooManyNodes
  | otherwise = nodes k # arrows k # bgFrame 0.4 white
  where
    tooFewNodes =
      text "Too few nodes requested" # fontSizeL 0.05 # bgFrame 0.4 red

    tooManyNodes =
      text "Too many nodes requested" # fontSizeL 0.05 # bgFrame 0.4 red

    nodes n =
      atPoints (trailVertices $ regPoly n 1) (node <$> [1..])

    arrowOpts =
      -- `with` is a name for "default settings", which get modified
      -- lens-style.
      with
      & gaps .~ small
      & headLength .~ local 0.15
      -- & arrowHead .~ noHead
      -- & arrowTail .~ noTail

    arrows (n :: Int) =
      -- connectOutside' (note prime) is a version of connectOutside
      -- that allows for arrow customization in the first argument.
      mconcat
      [ connectOutside' arrowOpts from' to'
      | from' <- [1..n]
      , to'   <- [from'..n]
      ]

    node (n :: Int) =
      text (show n) # fontSizeL 0.2  # fc white
      <>
      circle 0.2 # fc green # named n -- KEY HERE, with `named`
