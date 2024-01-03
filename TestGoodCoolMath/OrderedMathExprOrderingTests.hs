module OrderedMathExprOrderingTests ( spec ) where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( Gen, property )
import Test.Hspec.Checkers ( testBatch )
import Test.QuickCheck.Classes ( ord )
import GoodCoolMath ( MathExpr(..), OrderedMathExpr(..) )

spec :: Spec
spec =
  describe "Ordered math expression newtype ordering" $
    testBatch (ord (return :: OrderedMathExpr -> Gen OrderedMathExpr))
