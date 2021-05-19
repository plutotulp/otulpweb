{-# language ViewPatterns #-}

module OtulpWeb.Common.RotTest
  ( -- * Tests
    tests

    -- * Utilities
  , ValidChar(..)
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Test.Tasty
import Test.Tasty.HUnit

import Data.Sequence (Seq)
import Data.Maybe (isNothing, isJust)
import Test.Tasty.QuickCheck as QC

import qualified OtulpWeb.Common.Rot as Rot

tests :: TestTree
tests = testGroup "OtulpWeb.Common.RotTest" [units, properties]

units :: TestTree
units =
  testGroup "unit tests"
  (toList intCharConvValidInput)

properties :: TestTree
properties =
  testGroup "properties"
  [ propIntToCharDomain
  , propCharToIntDomain
  , propRotEncIsUppercase
  , propRotDecIsLowercase
  , propRotEncAndRotDecInverse
  , propRotDecAndRotEncInverse
  , propRotEncStringDropsInvalidChars
  , propRotEncStringAndRotDecStringInverse
  , propRotDecStringAndRotEncStringInverse
  ]

-- Character whose Arbitrary instance is guaranteed to generated
-- characters valid for Rot encoding.
newtype ValidChar = ValidChar { getValidChar :: Char } deriving (Show)

instance Arbitrary ValidChar where
  arbitrary = ValidChar <$> QC.elements (toList Rot.validChars)

propIntToCharDomain :: TestTree
propIntToCharDomain = QC.testProperty lab prop
  where
    lab =
      "intToChar domain"
    prop i =
      let
        check =
          if Rot.isValidCharInt i then isJust else isNothing
      in
        check (Rot.intToChar i)

propCharToIntDomain :: TestTree
propCharToIntDomain = QC.testProperty lab prop
  where
    lab =
      "charToInt domain"
    prop c =
      let
        check =
          if Rot.isValidChar c then isJust else isNothing
      in
        check (Rot.charToInt c)

intCharConvValidInput :: Seq TestTree
intCharConvValidInput = mkUnitIntCharConv <$> Rot.validCharInts

mkUnitIntCharConv :: Int -> TestTree
mkUnitIntCharConv i =
  testCase lab $ (Rot.intToChar i >>= Rot.charToInt) @?= Just i
  where
    lab =
      "intToChar " ++ show i ++ " >>= charToInt == Just " ++ show i

propRotEncIsUppercase :: TestTree
propRotEncIsUppercase = QC.testProperty lab prop
  where
    lab = "rotEnc yields uppercase"
    prop k (getValidChar -> c) =
      fmap isUpper (Rot.rotEnc k c) == Just True

propRotDecIsLowercase :: TestTree
propRotDecIsLowercase = QC.testProperty lab prop
  where
    lab = "rotDec yields lowercase"
    prop k (toUpper . getValidChar -> c) =
      fmap isLower (Rot.rotDec k c) == Just True

propRotEncAndRotDecInverse :: TestTree
propRotEncAndRotDecInverse = QC.testProperty lab prop
  where
    lab =
      "(rotEnc k >=> rotDec k) c == Just c, for valid c"
    prop k (getValidChar -> c) =
        (Rot.rotEnc k >=> Rot.rotDec k) c == Just c

propRotDecAndRotEncInverse :: TestTree
propRotDecAndRotEncInverse =
  QC.testProperty lab prop
  where
    lab =
      "(rotDec k >=> rotEnc k) c == Just c, for valid c"
    prop k (toUpper . getValidChar -> c) =
      (Rot.rotDec k >=> Rot.rotEnc k) c == Just c

propRotEncStringDropsInvalidChars :: TestTree
propRotEncStringDropsInvalidChars =
  QC.testProperty lab prop
  where
    lab =
      "rotEncString drops invalid chars"
    prop k str =
      length (Rot.rotEncString k str) == length (filter Rot.isValidChar str)

propRotEncStringAndRotDecStringInverse :: TestTree
propRotEncStringAndRotDecStringInverse =
  QC.testProperty lab prop
  where
    lab =
      "(rotDecString k . rotEncString k) str == str, for valid str"
    prop k (fmap getValidChar -> str) =
      (Rot.rotDecString k . Rot.rotEncString k) str == str

propRotDecStringAndRotEncStringInverse :: TestTree
propRotDecStringAndRotEncStringInverse =
  QC.testProperty lab prop
  where
    lab =
      "(rotEncString k . rotDecString k) str == str, for valid str"
    prop k (fmap (toUpper . getValidChar) -> str) =
      (Rot.rotEncString k . Rot.rotDecString k) str == str
