{-# LANGUAGE ScopedTypeVariables #-}
module SimplificationTests where

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
    describe "with an empty sub-expression list" $ do
      it "should simplify a sum to 0" $
        simplifyFully (Sum []) =~= IntLit 0 `shouldBe` True
      it "should simplify a product to 1" $
        simplifyFully (Prod []) =~= IntLit 1 `shouldBe` True
    describe "with a negative expression" $ do
      it "should change a double negative to a positive of the simplified subexpression" $ property $
        \ e -> e =->..<-= Neg (Neg e)
    describe "when a sum has a sum as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Sum (Sum es1 : es2) =->..<-= Sum (es1 ++ es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Sum (es1 ++ [Sum es2] ++ es3) =->..<-= Sum (es1 ++ es2 ++ es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Sum (es1 ++ [Sum es2]) =->..<-= Sum (es1 ++ es2)
    describe "when a sum has zero literals" $ do
      it "should remove it if there is only one" $ property $
        \ (es1', es2') -> Sum (map unwrapNonZeroIntLitMathExpr es1' ++ [IntLit 0] ++ map unwrapNonZeroIntLitMathExpr es2') =->..<-= Sum (map unwrapNonZeroIntLitMathExpr es1' ++ map unwrapNonZeroIntLitMathExpr es2')
      it "should remove them when there are many" $ property $
        \ (WithWithoutIntLitZeroMathExprs (xs, ys)) -> Sum xs =->..<-= Sum ys
    describe "when a product has a product as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Prod (Prod es1 : es2) =->..<-= Prod (es1 ++ es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Prod (es1 ++ [Prod es2] ++ es3) =->..<-= Prod (es1 ++ es2 ++ es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Prod (es1 ++ [Prod es2]) =->..<-= Prod (es1 ++ es2)
    describe "when a product has one literals" $ do
      it "should remove it if there is only one" $ property $
        \ (es1', es2') -> Prod (map unwrapNonOneIntLitMathExpr es1' ++ [IntLit 1] ++ map unwrapNonOneIntLitMathExpr es2') =->..<-= Prod (map unwrapNonOneIntLitMathExpr es1' ++ map unwrapNonOneIntLitMathExpr es2')
      it "should remove them when there are many" $ property $
        \ (WithWithoutIntLitOneMathExprs (xs, ys)) -> Prod xs =->..<-= Prod ys
    describe "with an exponential" $ do
      it "should simplify a logarithm exponent to the subexpression" $ property $
        \ e -> e =->..<-= Exp (Ln e)
      it "should simplify to product of exponentials given a sum exponent" $ property $
        \ es -> (Prod . map Exp) es =->..<-= Exp (Sum es)
