{-# LANGUAGE ScopedTypeVariables #-}
module SimplificationTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import GoodCoolMath
  ( simplifyFully
  , MathExpr(..)
  , (=~=)
  , (=->..<-=)
  , MathExprLeaf(..)
  , unwrapExprLeaf )

doesntSimplify :: MathExpr -> Bool
doesntSimplify e = ((=~= e) . simplifyFully) e

spec :: Spec
spec = do
  describe "Simplification" $ do
    describe "Where no simplification possible" $ do
      it "shouldn't simplify a leaf expression" $ property $
        \ (MathExprLeaf e) -> doesntSimplify e
      it "shouldn't simplify a negative node with a leaf" $ property $
        \ (MathExprLeaf e') -> (doesntSimplify . Neg) e'
      it "shouldn't simplify a sum node with all leaves" $ property $
        \ (leaves :: [MathExprLeaf]) -> null leaves || (doesntSimplify . Sum . map unwrapExprLeaf) leaves
      it "shouldn't simplify a product node with all leaves" $ property $
        \ (leaves :: [MathExprLeaf]) -> null leaves || (doesntSimplify . Prod . map unwrapExprLeaf) leaves
      it "shouldn't simplify a fraction of leaves" $ property $
        \ (MathExprLeaf e1, MathExprLeaf e2) -> doesntSimplify (Frac e1 e2)
      it "shouldn't simplify an exponential of a leaf" $ property $
        \ (MathExprLeaf e) -> (doesntSimplify . Exp) e
      it "shouldn't simplify a logarithm of a leaf" $ property $
        \ (MathExprLeaf e) -> (doesntSimplify . Ln) e
    describe "With an empty sub-expression list" $ do
      it "should simplify a sum to 0" $
        simplifyFully (Sum []) =~= IntLit 0 `shouldBe` True
      it "should simplify a product to 1" $
        simplifyFully (Prod []) =~= IntLit 1 `shouldBe` True
    describe "With a negative expression" $ do
      it "should change a double negative to a positive of the simplified subexpression" $ property $
        \ e -> e =->..<-= Neg (Neg e)
    describe "When a sum has a sum as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Sum (Sum es1 : es2) =->..<-= Sum (es1 ++ es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Sum (es1 ++ [Sum es2] ++ es3) =->..<-= Sum (es1 ++ es2 ++ es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Sum (es1 ++ [Sum es2]) =->..<-= Sum (es1 ++ es2)
    describe "When a product has a product as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Prod (Prod es1 : es2) =->..<-= Prod (es1 ++ es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Prod (es1 ++ [Prod es2] ++ es3) =->..<-= Prod (es1 ++ es2 ++ es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Prod (es1 ++ [Prod es2]) =->..<-= Prod (es1 ++ es2)
    describe "With an exponential" $ do
      it "should simplify a logarithm exponent to the subexpression" $ property $
        \ e -> e =->..<-= Exp (Ln e)
      it "should simplify to product of exponentials given a sum exponent" $ property $
        \ es -> (Prod . map Exp) es =->..<-= Exp (Sum es)
    -- TODO - more tests
