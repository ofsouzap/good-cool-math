{-# LANGUAGE ScopedTypeVariables #-}
module EvaluationTests ( spec ) where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe, SpecWith, Example (Arg) )
import Test.QuickCheck
  ( property, Property, Arbitrary )
import Utils
import GoodCoolMath ( MathExpr(..), (=~=), evalApprox, evalApproxWith, set, empty, sub, EvalEnv, EnvList, EnvTree )
import GoodCoolMath.Shorthand
import Data.Maybe (isNothing)
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NonEmpty ( map )

-- Utility functions

foldMaybes :: Foldable t => (a -> b -> b) -> b -> t (Maybe a) -> Maybe b
foldMaybes f a = foldr foldFunc (Just a) where
  foldFunc _ Nothing = Nothing
  foldFunc Nothing _ = Nothing
  foldFunc (Just x) (Just acc) = Just (f x acc)

sumMaybes :: (Foldable t, Num a) => t (Maybe a) -> Maybe a
sumMaybes = foldMaybes (+) 0

productMaybes :: (Foldable t, Num a) => t (Maybe a) -> Maybe a
productMaybes = foldMaybes (*) 1

-- Testing properties

prop_evalEnvEmptySetThenSub :: (Show t, Arbitrary t, EvalEnv t) => t -> SpecWith (Arg Property)
prop_evalEnvEmptySetThenSub (_ :: t) = it "should return a set value after setting it from an empty environment" $ property $
  \ vName vVal -> Just vVal == (sub vName . set vName vVal) (empty :: t)

prop_evalEnvSetThenSub :: (Show t, Arbitrary t, EvalEnv t) => t -> SpecWith (Arg Property)
prop_evalEnvSetThenSub (_ :: t) = it "should return a set value after setting it from any environment" $ property $
  \ (env :: t) vName vVal -> Just vVal == (sub vName . set vName vVal) env

prop_emptyNothingEval :: (Show t, Arbitrary t, EvalEnv t) => t -> SpecWith (Arg Property)
prop_emptyNothingEval (_ :: t) = it "should not have a substitution for any variable from an empty environment" $ property $
  \ vName -> (isNothing . sub vName) (empty :: t)

evalEnv :: (Show t, Arbitrary t, EvalEnv t) => t -> SpecWith (Arg Property)
evalEnv env = do
  describe "is valid evaluation environment" $ do
    prop_evalEnvEmptySetThenSub env
    prop_evalEnvSetThenSub env
    prop_emptyNothingEval env

-- Full tests

spec :: Spec
spec = do
  describe "Evaluating" $ do
    describe "Approximate evaluation" $ do
      describe "without environment" $ do
        -- Note, I'm not trying to evaluate arbitrary subexpressions since this often leads to infinities. Instead I'm just using leaf nodes
        it "should evaluate an integer literal to its value" $ property $
          \ n -> isJustWhichApproxEq (fromIntegral n) 1e-3 ((evalApprox . IntLit) n)
        it "should not be able to evaluate a variable" $ property $
          \ vName -> (isNothing . evalApprox . Var) vName
        it "should evaluate a negative to the negative of the evaluation of the subexpression" $ property $
          \ (MathExprLeaf e) -> maybesApproxEqWithTol (1e-1 :: Double) (evalApprox (Neg e)) ((fmap negate . evalApprox) e)
        it "should evaluate a sum to the sum of the evaluations of it's subexpression" $ property $
          \ (es :: NonEmpty MathExprLeaf) -> maybesApproxEqWithTol (1e-1 :: Double)
          (evalApprox ((Sum . NonEmpty.map unwrapExprLeaf) es))
          (sumMaybes (NonEmpty.map (evalApprox . unwrapExprLeaf) es))
        it "should evaluate a product to the product of the evaluations of it's subexpression" $ property $
          \ (es :: NonEmpty MathExprLeaf) -> maybesApproxEqWithTol (1e-1 :: Double)
          (evalApprox ((Prod . NonEmpty.map unwrapExprLeaf) es))
          (productMaybes (NonEmpty.map (evalApprox . unwrapExprLeaf) es))
        it "should evaluate a fraction to the division of the subexpressions" $ property $
          \ (MathExprLeaf num) (NotZeroIntLitMathExprLeaf den) -> maybesApproxEqWithTol (1e-1 :: Double)
          (evalApprox (Frac num den))
          (evalApprox num >>= \ x -> (fmap (x/) . evalApprox) den)
        it "should evaluate an exponential to the exponential of the evaluation of the subexpression" $ property $
          \ (MathExprLeaf e) -> maybesApproxEqWithTol (1e-1 :: Double)
          (evalApprox (Exp e))
          ((fmap exp . evalApprox) e)
        it "should evaluate a logarithm to the logarithm of the evaluation of the subexpression" $ property $
          \ (IntLitOnlyPosMathExprLeaf e) -> maybesApproxEqWithTol (1e-1 :: Double)
          (evalApprox (Ln e))
          ((fmap log . evalApprox) e)
        describe "should evaluate the hard-coded cases to roughly the expected results" $ do
          it "case 0" $
            shouldBeJustWhichApproxEq 1 1e-3 (evalApprox (half `plus` half `plus` (Exp (Ln two) `times` zero)))
          it "case 1 (tanh 0)" $
            shouldBeJustWhichApproxEq 0 1e-3 (let x = zero in evalApprox ((Exp x `minus` Exp (neg x)) `dividedBy` (Exp x `plus` Exp (neg x))))
          it "case 1 (tanh 5)" $
            shouldBeJustWhichApproxEq 0.99990920426 1e-3 (let x = int 5 in evalApprox ((Exp x `minus` Exp (neg x)) `dividedBy` (Exp x `plus` Exp (neg x))))
      describe "with environment" $ do
        it "should evaluate an integer literal to its value" $ property $
          \ n (env :: EnvList) -> isJustWhichApproxEq (fromIntegral n) 1e-3 ((evalApproxWith env . IntLit) n)
        it "should evaluate a defined variable to its environment-defined value when it is the only defined variable" $ property $
          \ vName vVal -> let (env :: EnvList) = set vName vVal empty in isJustWhichApproxEq (fromIntegral vVal) 1e-3 ((evalApproxWith env . Var) vName)
        it "should not be able to evaluate a variable using an empty environment" $ property $
          \ vName (vVal :: Int) -> (isNothing . evalApproxWith (empty :: EnvList) . Var) vName
        -- TODO - hard-coded tests with variable substitutions needed
    describe "evaluation environments" $ do
      evalEnv (empty :: EnvList)
    describe "evaluation environments" $ do
      evalEnv (empty :: EnvTree)
