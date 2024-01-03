{-# LANGUAGE ScopedTypeVariables #-}
module EvaluationTests ( spec ) where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Utils
import GoodCoolMath ( MathExpr(..), (=~=), evalApprox, evalApproxWith, EnvList(EnvList) )
import GoodCoolMath.Shorthand

spec :: Spec
spec = do
  describe "Evaluating" $ do
    describe "Approximate Evaluation" $ do
      it "should evaluate an integer literal to its value without an environment" $ property $
        \ n -> (evalApprox . IntLit) n == (Just . fromIntegral) n
      it "should evaluate an integer literal to its value with any environment" $ property $
        \ n (env :: EnvList) -> (evalApproxWith env . IntLit) n == (Just . fromIntegral) n
      -- TODO - test evaluating variables when variable is defined
      -- TODO - test evaluating variables when variable is not defined
      describe "should evaluate the hard-coded cases to roughly the expected results" $ do
        it "case 0" $
          maybeShouldApproxEq 1 (1e-3 :: Double) (evalApprox (half `plus` half `plus` (Exp (Ln two) `times` zero)))
        -- TODO - more hard-coded test cases
    -- TODO - test environment substitution and value-setting
