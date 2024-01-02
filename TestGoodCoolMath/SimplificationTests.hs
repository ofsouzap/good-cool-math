{-# LANGUAGE ScopedTypeVariables #-}
module SimplificationTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it )
import Test.QuickCheck
  ( property )
import GoodCoolMath
  ( simplifyFully
  , MathExpr(..)
  , MathExprLeaf(..)
  , unwrapExprLeaf )

doesntSimplify :: MathExpr -> Bool
doesntSimplify e = ((== e) . simplifyFully) e

spec :: Spec
spec = do
  describe "Where no simplification possible" $ do
    it "shouldn't simplify a leaf expression" $ property $
      \ (MathExprLeaf e) -> doesntSimplify e
    it "shouldn't simplify a negative node with a leaf" $ property $
      \ (MathExprLeaf e') -> (doesntSimplify . Neg) e'
    it "shouldn't simplify a sum node with all leaves" $ property $
      \ (leaves :: [MathExprLeaf]) -> (doesntSimplify . Sum . map unwrapExprLeaf) leaves
    it "shouldn't simplify a product node with all leaves" $ property $
      \ (leaves :: [MathExprLeaf]) -> (doesntSimplify . Prod . map unwrapExprLeaf) leaves
    it "shouldn't simplify a fraction of leaves" $ property $
      \ (MathExprLeaf e1, MathExprLeaf e2) -> doesntSimplify (Frac e1 e2)
    it "shouldn't simplify an exponential of a leaf" $ property $
      \ (MathExprLeaf e) -> (doesntSimplify . Exp) e
    it "shouldn't simplify a logarithm of a leaf" $ property $
      \ (MathExprLeaf e) -> (doesntSimplify . Ln) e
  -- TODO - test functionality where it actually simplifies stuff
