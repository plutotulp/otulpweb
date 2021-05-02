{-# language ViewPatterns #-}

module OtulpWeb.Common.Vigenere
  ( -- * Encryption
    VigKey(..)
  , mkVigKey
  , ppVigKey
  , vigEncString
  , vigDecString
  ) where

import Data.Foldable
import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import OtulpWeb.Common.Rot (RotKey(..))
import qualified OtulpWeb.Common.Rot as Rot

newtype VigKey = VigKey { unVigKey :: Seq RotKey }
  deriving (Eq, Show)

mkVigKey :: String -> VigKey
mkVigKey = VigKey . Seq.fromList . mapMaybe Rot.charToRotKey

ppVigKey :: VigKey -> String
ppVigKey =
  mapMaybe (Rot.intToChar . Rot.unRotKey) . toList . unVigKey

vigKeyToRotKeys :: VigKey -> [RotKey]
vigKeyToRotKeys (unVigKey -> ks)
  | null ks   = repeat 0
  | otherwise = cycle (toList ks)

vigEncString :: VigKey -> String -> String
vigEncString (vigKeyToRotKeys -> ks) str =
  catMaybes (zipWith Rot.rotEnc ks str)

vigDecString :: VigKey -> String -> String
vigDecString (vigKeyToRotKeys -> ks) str =
  catMaybes (zipWith Rot.rotDec ks str)
