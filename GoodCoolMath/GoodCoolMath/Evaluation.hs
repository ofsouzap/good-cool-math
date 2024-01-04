module GoodCoolMath.Evaluation
  ( EvalEnv(..)
  , EnvList
  , EnvTree
  , evalApprox
  , evalApproxWith ) where

import Test.QuickCheck ( Arbitrary(arbitrary), Gen, frequency, suchThatMaybe )
import Data.List.NonEmpty
  ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import GoodCoolMath.Expressions
  ( Const(..)
  , VarName(..)
  , MathExpr(..) )
import Data.Maybe (fromMaybe)

------------------------------
-- Evalutation Environments --
------------------------------

-- | An evaluation environment, capable of substituting constants for variables
class EvalEnv t where
  -- | The environment with no variables' values set
  empty :: t
  -- | Try find a value to substitute for a variable name in the environment
  sub :: VarName -> t -> Maybe Const
  -- | Set a value for a variable name in the environment
  set :: VarName -> Const -> t -> t
  -- | Substitute for all variables possible in an expression using the environment
  subExpr :: MathExpr -> t -> MathExpr
  subExpr e@(Const _) _ = e
  subExpr e@(Var vName) env = (maybe e Const . sub vName) env
  subExpr (Neg e) env = (Neg . subExpr e) env
  subExpr (Sum es) env = (Sum . NonEmpty.map (`subExpr` env)) es
  subExpr (Prod es) env = (Prod . NonEmpty.map (`subExpr` env)) es
  subExpr (Frac num den) env = Frac (subExpr num env) (subExpr den env)
  subExpr (Exp e) env = Exp (subExpr e env)
  subExpr (Ln e) env = Ln (subExpr e env)

-- EnvList

-- | An evaluation environment stored as a list of variable name-value pairs.
-- Values are added in constant time by appending them to the head of the list
-- but, consequently, the list can become very long if values are frequently changed
newtype EnvList = EnvList [(VarName, Const)]
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

-- EnvTree

-- | An evaluation environment stored as a binary search tree with variable name-value pairs.
-- In the average case, this will have logarithmic-time insertions and queries
-- but, in the worst case, can have linear-time insertions and queries
data EnvTree =
    Leaf
  | Node EnvTree EnvTree (VarName, Const)
  deriving ( Show )

instance EvalEnv EnvTree where
  empty = Leaf
  sub _ Leaf = Nothing
  sub xn (Node l r (nn, nv))
    | xn < nn = sub xn l
    | xn > nn = sub xn r
    | otherwise = Just nv
  set xn xv Leaf = Node Leaf Leaf (xn, xv)
  set xn xv (Node l r (nn, nv))
    | xn < nn = Node (set xn xv l) r (nn, nv)
    | xn > nn = Node l (set xn xv r) (nn, nv)
    | otherwise = Node l r (nn, xv)

instance Arbitrary EnvTree where
  arbitrary = arbitraryConstrained Nothing
    where
      getVarName :: EnvTree -> Maybe VarName
      getVarName Leaf = Nothing
      getVarName (Node _ _ (x,_)) = Just x
      arbitraryConstrained :: Maybe (Ordering, VarName) -> Gen EnvTree
      arbitraryConstrained Nothing = do
        childName <- (arbitrary :: Gen VarName)
        frequency [(1, return Leaf), (1, arbitraryNodeWithName childName)]
      arbitraryConstrained (Just (LT, s)) = fromMaybe Leaf <$> suchThatMaybe (arbitrary :: Gen EnvTree) (maybe True (< s) . getVarName)
      arbitraryConstrained (Just (GT, s)) = fromMaybe Leaf <$> suchThatMaybe (arbitrary :: Gen EnvTree) (maybe True (> s) . getVarName)
      arbitraryConstrained (Just (EQ, s)) = arbitraryNodeWithName s
      arbitraryNodeWithName :: VarName -> Gen EnvTree
      arbitraryNodeWithName s = do
        l <- arbitraryConstrained (Just (LT, s))
        r <- arbitraryConstrained (Just (GT, s))
        val <- (arbitrary :: Gen Const)
        return (Node l r (s, val))

----------------
-- Evaluating --
----------------

safeLog :: (Ord t, Floating t) => t -> Maybe t
safeLog x
  | x <= 0 = Nothing
  | otherwise = (Just . log) x

-- | Evalutate an expression.
-- If the expression still contains any named values (constant or variable), Nothing is returned.
-- This evaluation function is approximate but can fully evaluate any expression without variables
evalApprox :: (Ord t, Floating t) => MathExpr -> Maybe t
evalApprox (Const (IntLit n)) = (Just . fromIntegral) n
evalApprox (Const (NamedConst _)) = Nothing
evalApprox (Const UnnamedConst) = Nothing
evalApprox (Const Pi) = Just pi
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
