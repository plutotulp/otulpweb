import Test.Tasty

import qualified OtulpWeb.Common.RotTest as Rot
import qualified OtulpWeb.Common.VigenereTest as Vig

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "top"
  [ Rot.tests
  , Vig.tests
  ]
