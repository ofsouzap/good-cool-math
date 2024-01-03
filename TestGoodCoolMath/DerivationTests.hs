{-# LANGUAGE ScopedTypeVariables #-}
module DerivationTests ( spec ) where

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
  ( MathExpr(..)
  , (=->..<-=)
  , der
  , zero
  , one
  , negOne
  , plus
  , minus
  , times
  , dividedBy
  , sqr )
import Data.List.NonEmpty
  ( NonEmpty
  , fromList )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )

spec :: Spec
spec = do
  describe "Derivation" $ do
    describe "with leaf nodes" $ do
      it "should return derivative of an integer as zero" $ property $
        \ dVar n -> der dVar (IntLit n) =->..<-= zero
      it "should return derivative of variable as zero if the variable is not the derivative variable" $ property $
        \ dVar vName -> dVar == vName || der dVar (Var vName) =->..<-= zero
      it "should return derivative of the differentiation variable as one" $ property $
        \ var -> der var (Var var) =->..<-= one
    it "should return derivative of negative as negative of the derivative" $ property $
      \ dVar e -> der dVar (Neg e) =->..<-= Neg (der dVar e)
    it "should return derivative of sum as sum of derivatives" $ property $
      \ dVar (es :: NonEmpty MathExpr) -> der dVar (Sum es) =->..<-= Sum (NonEmpty.map (der dVar) es)
    -- TODO - test derivative of product
    it "should return correct derivative of a fraction" $ property $
      \ dVar num den -> der dVar (Frac num den) =->..<-= (((den `times` der dVar num) `minus` (num `times` der dVar den)) `dividedBy` sqr den)
    it "should return the correct derivative of an exponential" $ property $
      \ dVar e -> der dVar (Exp e) =->..<-= (e `times` Exp (e `plus` negOne) `times` der dVar e)
    it "should return the correct derivative of a logarithm" $ property $
      \ dVar e -> der dVar (Ln e) =->..<-= Frac (der dVar e) e
