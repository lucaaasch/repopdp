import Test.Hspec
import PdePreludat
import qualified SpecTP0
import qualified SpecTP1
import qualified SpecTP2

main :: IO ()
main = hspec $ do
  describe "TP0"  SpecTP0.spec
  describe "TP1"  SpecTP1.spec
  describe "TP2"  SpecTP2.spec