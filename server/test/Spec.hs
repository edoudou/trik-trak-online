import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import qualified Data.GameSpec
import qualified GameSpec

main :: IO ()
main = do

  spec1 <- testSpec "Data.GameSpec" Data.GameSpec.spec
  spec2 <- testSpec "GameSpec" GameSpec.spec

  defaultMain $
    testGroup "tests"
      [ spec1
      , spec2
      ]
