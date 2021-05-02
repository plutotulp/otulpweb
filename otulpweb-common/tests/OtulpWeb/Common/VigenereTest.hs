{-# language ViewPatterns #-}

module OtulpWeb.Common.VigenereTest (tests) where

-- import Control.Monad
import Data.Char
-- import Data.Foldable
import Test.Tasty
-- import Test.Tasty.HUnit

-- import Data.Sequence (Seq)
-- import Data.Maybe (isNothing, isJust)
-- import Test.Tasty.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck as QC

import OtulpWeb.Common.RotTest (ValidChar(..))
import OtulpWeb.Common.Vigenere (VigKey)
import qualified OtulpWeb.Common.Vigenere as Vig

tests :: TestTree
tests = testGroup "OtulpWeb.Common.VigenereTest" [properties]

properties :: TestTree
properties =
  testGroup "properties"
  [ propVigEncStringAndVigDecStringInverse
  , propVigDecStringAndVigEncStringInverse
  ]

getValidVigKey :: [ValidChar] -> VigKey
getValidVigKey = Vig.mkVigKey . getValidString

getValidString :: [ValidChar] -> String
getValidString = fmap getValidChar

propVigEncStringAndVigDecStringInverse :: TestTree
propVigEncStringAndVigDecStringInverse =
  QC.testProperty lab prop
  where
    lab =
      "(vigDecString k . vigEncString k) str == str, for valid str"
    prop (getValidVigKey -> k) (getValidString -> str) =
      (Vig.vigDecString k . Vig.vigEncString k) str == str

propVigDecStringAndVigEncStringInverse :: TestTree
propVigDecStringAndVigEncStringInverse =
  QC.testProperty lab prop
  where
    lab =
      "(vigEncString k . vigDecString k) str == str, for valid str"
    prop (getValidVigKey -> k) (fmap toUpper . getValidString -> str) =
      (Vig.vigEncString k . Vig.vigDecString k) str == str
