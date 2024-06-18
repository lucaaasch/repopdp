import Test.Hspec
import PdePreludat
import qualified SpecTP0

main :: IO ()
main = hspec $ do
  describe "TP0"  SpecTP0.spec