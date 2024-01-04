module GoodCoolMath.Evaluation
  ( EvalEnv(..)
  , EnvList(..)
  , evalApprox
  , evalApproxWith ) where

import Test.QuickCheck ( Arbitrary(arbitrary) )
import Data.List.NonEmpty
  ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import GoodCoolMath.Expressions
  ( MathExpr(..) )

------------------------------
-- Evalutation Environments --
------------------------------

class EvalEnv t where
  -- | The environment with no variables' values set
  empty :: t
  -- | Try find a value to substitute for a variable name in the environment
  sub :: String -> t -> Maybe Int
  -- | Set a value for a variable name in the environment
  set :: String -> Int -> t -> t
  -- | Substitute for all variables possible in an expression using the environment
  subExpr :: MathExpr -> t -> MathExpr
  subExpr e@(IntLit _) _ = e
  subExpr e@(Var vName) env = (maybe e IntLit . sub vName) env
  subExpr (Neg e) env = (Neg . subExpr e) env
  subExpr (Sum es) env = (Sum . NonEmpty.map (`subExpr` env)) es
  subExpr (Prod es) env = (Prod . NonEmpty.map (`subExpr` env)) es
  subExpr (Frac num den) env = Frac (subExpr num env) (subExpr den env)
  subExpr (Exp e) env = Exp (subExpr e env)
  subExpr (Ln e) env = Ln (subExpr e env)

newtype EnvList = EnvList [(String, Int)]
  deriving ( Show )

instance EvalEnv EnvList where
  empty = EnvList []
  sub _ (EnvList []) = Nothing
  sub n (EnvList ((hn,hv):ts))
    | hn == n = Just hv
    | otherwise = sub n (EnvList ts)
  set n v (EnvList xs) = EnvList ((n,v):xs)

instance Arbitrary EnvList where
  arbitrary = EnvList <$> arbitrary

-- TODO - binary search tree evaluation environment

----------------
-- Evaluating --
----------------

safeLog :: (Ord t, Floating t) => t -> Maybe t
safeLog x
  | x <= 0 = Nothing
  | otherwise = (Just . log) x

-- | Evalutate an expression.
-- If the expression still contains any variables, Nothing is returned.
-- This evaluation function is approximate but can fully evaluate any expression without variables
evalApprox :: (Ord t, Floating t) => MathExpr -> Maybe t
evalApprox (IntLit n) = (Just . fromIntegral) n
evalApprox (Var _) = Nothing
evalApprox (Neg e) = negate <$> evalApprox e
evalApprox (Sum (h:|[])) = evalApprox h
evalApprox (Sum es) = foldr foldFunc (Just 0) es where
  foldFunc e = (=<<) (flip fmap (evalApprox e) . (+))
evalApprox (Prod es) = foldr foldFunc (Just 1) es where
  foldFunc e = (=<<) (flip fmap (evalApprox e) . (*))
evalApprox (Frac num den) = do
  x <- evalApprox num
  y <- evalApprox den
  return (x / y)
evalApprox (Exp e) = exp <$> evalApprox e
evalApprox (Ln e) = safeLog =<< evalApprox e

-- | Evaluate an expression using an evaluation environment
evalApproxWith :: (Ord t, Floating t, EvalEnv e) => e -> MathExpr -> Maybe t
evalApproxWith env = evalApprox . (`subExpr` env)

-- TODO - use subtitution to exaclty evaluate (no approximation, keep to integers) expressions as much as possible
