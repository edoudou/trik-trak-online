module SpecUtils
  ( assertAllPlayerIds
  , allPlayerIds
  )
  where

import qualified Data.Set         as S
import           Test.Tasty.Hspec (Expectation, Spec, shouldBe)

import           Data.Game        (GameResult (..), PlayerId (..))

allPlayerIds :: [PlayerId]
allPlayerIds = [P1, P2, P3, P4]

assertAllPlayerIds :: [GameResult] -> Expectation
assertAllPlayerIds xs =
  let pids = (\(NewPlayer _ pid) -> pid) <$> xs
  in S.fromList pids `shouldBe` S.fromList [P1, P2, P3, P4]
