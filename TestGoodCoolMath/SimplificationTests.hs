{-# LANGUAGE ScopedTypeVariables #-}
module SimplificationTests ( spec ) where

import Data.List.NonEmpty
  ( (<|) )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property, suchThat )
import Utils
import GoodCoolMath
  ( simplifyFully
  , MathExpr(..)
  , (=~=)
  , (=->..<-=)
  , MathExprLeaf(..)
  , unwrapExprLeaf
  , isIntLitOf )
import GoodCoolMath.Shorthand

doesntSimplify :: MathExpr -> Bool
doesntSimplify e = ((=~= e) . simplifyFully) e

spec :: Spec
spec = do
  describe "Simplification" $ do
    describe "where no simplification possible" $ do
      it "shouldn't simplify a leaf expression" $ property $
        \ (MathExprLeaf e) -> doesntSimplify e
      it "shouldn't simplify a negative node with a leaf" $ property $
        \ (MathExprLeaf e') -> (doesntSimplify . Neg) e'
      it "shouldn't simplify a fraction of leaves" $ property $
        \ (MathExprLeaf e1, MathExprLeaf e2) -> doesntSimplify (Frac e1 e2)
      it "shouldn't simplify an exponential of a leaf" $ property $
        \ (MathExprLeaf e) -> (doesntSimplify . Exp) e
      it "shouldn't simplify a logarithm of a leaf" $ property $
        \ (MathExprLeaf e) -> (doesntSimplify . Ln) e
    describe "with a negative expression" $ do
      it "should change a double negative to a positive of the simplified subexpression" $ property $
        \ e -> e =->..<-= Neg (Neg e)
    describe "when a sum has a sum as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Sum (Sum es1 <| es2) =->..<-= Sum (es1 <> es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Sum (es1 <> pure (Sum es2) <> es3) =->..<-= Sum (es1 <> es2 <> es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Sum (es1 <> pure (Sum es2)) =->..<-= Sum (es1 <> es2)
    describe "when a sum has zero literals" $ do
      it "should remove it if there is only one" $ property $
        \ (es1', es2') -> Sum (NonEmpty.map unwrapNonZeroIntLitMathExpr es1' <> pure (zero) <> NonEmpty.map unwrapNonZeroIntLitMathExpr es2') =->..<-= Sum (NonEmpty.map unwrapNonZeroIntLitMathExpr es1' <> NonEmpty.map unwrapNonZeroIntLitMathExpr es2')
      it "should remove them when there are many" $ property $
        \ (WithWithoutIntLitZeroMathExprs (xs, ys)) -> Sum xs =->..<-= Sum ys
    -- TODO - test combining int literals in sum
    describe "when a product has a product as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Prod (Prod es1 <| es2) =->..<-= Prod (es1 <> es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Prod (es1 <> pure (Prod es2) <> es3) =->..<-= Prod (es1 <> es2 <> es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Prod (es1 <> pure (Prod es2)) =->..<-= Prod (es1 <> es2)
    describe "when a product has one literals" $ do
      it "should remove it if there is only one" $ property $
        \ (es1', es2') -> Prod (NonEmpty.map unwrapNonOneIntLitMathExpr es1' <> pure (one) <> NonEmpty.map unwrapNonOneIntLitMathExpr es2') =->..<-= Prod (NonEmpty.map unwrapNonOneIntLitMathExpr es1' <> NonEmpty.map unwrapNonOneIntLitMathExpr es2')
      it "should remove them when there are many" $ property $
        \ (WithWithoutIntLitOneMathExprs (xs, ys)) -> Prod xs =->..<-= Prod ys
    -- TODO - test combining int literals in product
    -- describe "when a product has multiple sums in it" $ do
    --   it "should expand them into a single sum" $ property $
    --     \ (SumsAndWithWithout (sumTerms, xs, others)) -> Prod xs =->..<-= Prod ((Sum . concat) sumTerms : others)
    describe "with an exponential" $ do
      it "should simplify a logarithm exponent to the subexpression" $ property $
        \ e -> e =->..<-= Exp (Ln e)
      it "should simplify to product of exponentials given a sum exponent" $ property $
        \ es -> (Prod . NonEmpty.map Exp) es =->..<-= Exp (Sum es)
