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
import GoodCoolMath ( MathExpr(..), OrderedConst(..), OrderedMathExpr(..) )

spec :: Spec
spec =
  describe "Ordered math expression newtype ordering" $ do
    testBatch (ord (return :: OrderedConst -> Gen OrderedConst))
    testBatch (ord (return :: OrderedMathExpr -> Gen OrderedMathExpr))
