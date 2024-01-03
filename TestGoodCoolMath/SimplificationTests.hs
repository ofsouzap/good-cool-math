{-# LANGUAGE ScopedTypeVariables #-}
module SimplificationTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property, suchThat, Arbitrary (arbitrary) )
import GoodCoolMath
  ( simplifyFully
  , MathExpr(..)
  , (=~=)
  , (=->..<-=)
  , MathExprLeaf(..)
  , unwrapExprLeaf, simplifyAtMost )

isIntLitOf :: Int -> MathExpr -> Bool
isIntLitOf n (IntLit x) = x == n
isIntLitOf _ _ = False

-- MathExpr that isn't IntLit 0

newtype NonZeroIntLitMathExpr = NonZeroIntLitMathExpr MathExpr
  deriving ( Show )

unwrapNonZeroIntLitMathExpr :: NonZeroIntLitMathExpr -> MathExpr
unwrapNonZeroIntLitMathExpr (NonZeroIntLitMathExpr e) = e

instance Arbitrary NonZeroIntLitMathExpr where
  arbitrary = NonZeroIntLitMathExpr <$> suchThat arbitrary (not . isIntLitOf 0)

-- MathExpr that isn't IntLit 1

newtype NonOneIntLitMathExpr = NonOneIntLitMathExpr MathExpr
  deriving ( Show )

unwrapNonOneIntLitMathExpr :: NonOneIntLitMathExpr -> MathExpr
unwrapNonOneIntLitMathExpr (NonOneIntLitMathExpr e) = e

instance Arbitrary NonOneIntLitMathExpr where
  arbitrary = NonOneIntLitMathExpr <$> suchThat arbitrary (not . isIntLitOf 0)

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
      it "shouldn't simplify a sum node with all leaves unless it is an integer literal 0" $ property $
        \ (leaves :: [MathExprLeaf]) -> null leaves || any (isIntLitOf 0 . unwrapExprLeaf) leaves || (doesntSimplify . Sum . map unwrapExprLeaf) leaves
      it "shouldn't simplify a product node with all leaves unless it is an integer literal 1" $ property $
        \ (leaves :: [MathExprLeaf]) -> null leaves || any (isIntLitOf 1 . unwrapExprLeaf) leaves || (doesntSimplify . Prod . map unwrapExprLeaf) leaves
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
    describe "when a product has a product as a child" $ do
      it "should expand it if it is the first item" $ property $
        \ es1 es2 -> Prod (Prod es1 : es2) =->..<-= Prod (es1 ++ es2)
      it "should expand it if it is somewhere within the list" $ property $
        \ es1 es2 es3 -> Prod (es1 ++ [Prod es2] ++ es3) =->..<-= Prod (es1 ++ es2 ++ es3)
      it "should expand it if it is the last item" $ property $
        \ es1 es2 -> Prod (es1 ++ [Prod es2]) =->..<-= Prod (es1 ++ es2)
    describe "with an exponential" $ do
      it "should simplify a logarithm exponent to the subexpression" $ property $
        \ e -> e =->..<-= Exp (Ln e)
      it "should simplify to product of exponentials given a sum exponent" $ property $
        \ es -> (Prod . map Exp) es =->..<-= Exp (Sum es)
    -- TODO - more tests
