{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}

module OtulpWeb.Common.Rot
  (  -- * Utilities
    validChars
  , validCharInts
  , isValidChar
  , isValidCharInt
  , charToInt
  , charToRotKey
  , intToChar
  , validText

  -- * Encryption
  , RotKey(..)
  , rotEnc
  , rotEncString
  , rotDec
  , rotDecString
  ) where

import Data.Char
import Data.Maybe

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Test.QuickCheck (Arbitrary)

newtype RotKey = RotKey { unRotKey :: Int }
  deriving (Arbitrary, Eq, Num, Ord, Show)

validCharInts :: Seq Int
validCharInts = [0..28]

isValidCharInt :: Int -> Bool
isValidCharInt i = isJust (Seq.elemIndexL i validCharInts)

validChars :: Seq Char
validChars = ['a'..'z'] <> ['æ', 'ø', 'å']

isValidChar :: Char -> Bool
isValidChar c = isJust (Seq.elemIndexL c validChars)

numValidChars :: Int
numValidChars = Seq.length validChars

aInt :: Int
aInt = fromEnum 'a'

charToInt :: Char -> Maybe Int
charToInt (fromEnum -> i)
  | i == æInt = Just 26
  | i == øInt = Just 27
  | i == åInt = Just 28
  | i < aInt || zInt < i = Nothing
  | otherwise = Just $! i - aInt
  where
    zInt = fromEnum 'z'
    æInt = fromEnum 'æ'
    øInt = fromEnum 'ø'
    åInt = fromEnum 'å'

charToRotKey :: Char -> Maybe RotKey
charToRotKey = fmap RotKey . charToInt

intToChar :: Int -> Maybe Char
intToChar i
  | i == 26 = Just 'æ'
  | i == 27 = Just 'ø'
  | i == 28 = Just 'å'
  | i < 0 || 28 < i = Nothing
  | otherwise = Just $! toEnum (i + aInt)

rot :: RotKey -> Int -> Int
rot (RotKey n) i = (n + i) `mod` numValidChars

validText :: String -> String
validText = filter isValidChar

-- ROT-encrypt lowercase Char into uppercase Char.
rotEnc :: RotKey -> Char -> Maybe Char
rotEnc k c = toUpper <$> ((rot k <$> charToInt c) >>= intToChar)

-- ROT-decrypt uppercase Char into lowercase Char.
rotDec :: RotKey -> Char -> Maybe Char
rotDec k (toLower -> c) = (rot (negate k) <$> charToInt c) >>= intToChar

rotEncString :: RotKey -> String -> String
rotEncString k str = catMaybes (rotEnc k <$> str)

rotDecString :: RotKey -> String -> String
rotDecString k str = catMaybes (rotDec k <$> str)
