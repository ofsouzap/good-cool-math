module Main where

import Test.Hspec ( hspec )
import qualified SimplificationTests ( spec )
import qualified EquatingTests ( spec )
import qualified OrderedMathExprOrderingTests ( spec )
import qualified DerivationTests ( spec )
import qualified EvaluationTests ( spec )

main :: IO ()
main = hspec $ do
  OrderedMathExprOrderingTests.spec
  EquatingTests.spec
  SimplificationTests.spec
  DerivationTests.spec
  EvaluationTests.spec
