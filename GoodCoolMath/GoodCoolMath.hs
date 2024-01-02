{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module GoodCoolMath
  ( MathExpr(..)
  , MathExprLeaf(..)
  , unwrapExprLeaf
  , simplifyAtMost
  , simplifyFully ) where

import Data.List ( intercalate, unfoldr )
import Test.QuickCheck
  ( Arbitrary
  , arbitrary
  , elements )

data MathExpr =
    IntLit Int
  | Var String
  | Neg MathExpr
  | Sum [MathExpr]
  | Prod [MathExpr]
  | Frac MathExpr MathExpr
  | Exp MathExpr
  | Ln MathExpr
  deriving (Eq)

instance Show MathExpr where
  show (IntLit x) = show x
  show (Var vName) = vName
  show (Neg m) = "-" ++ bracketedShow m
  show (Sum m) = (intercalate "+" . map bracketedShow) m
  show (Prod m) = (intercalate "*" . map bracketedShow) m
  show (Frac num den) = bracketedShow num ++ "/" ++ bracketedShow den
  show (Exp x) = "e^" ++ bracketedShow x
  show (Ln x) = "ln" ++ bracketedShow x

instance Arbitrary MathExpr where
  arbitrary = do
    eIntLit <- arbitrary >>= return . IntLit
    eVar <- arbitrary >>= return . Var
    eNeg <- arbitrary >>= return . Neg
    eSum <- arbitrary
    eProd <- arbitrary
    eFrac <- arbitrary >>= return . uncurry Frac
    eExp <- arbitrary >>= return . Exp
    eLn <- arbitrary >>= return . Ln
    elements
      [ eIntLit
      , eVar
      , eNeg
      , eSum
      , eProd
      , eFrac
      , eExp
      , eLn ]

-- Expression leaf nodes

newtype MathExprLeaf = MathExprLeaf MathExpr

unwrapExprLeaf :: MathExprLeaf -> MathExpr
unwrapExprLeaf (MathExprLeaf e') = e'

instance Show MathExprLeaf where
  show (MathExprLeaf e') = show e'

instance Arbitrary MathExprLeaf where
  arbitrary = do
    eIntLit <- arbitrary >>= return . MathExprLeaf . IntLit
    eVar <- arbitrary >>= return . MathExprLeaf . Var
    elements [ eIntLit, eVar ]

-- Shortcuts for me

zero, one, negOne :: MathExpr
zero = IntLit 0
one = IntLit 1
negOne = Neg one

reciprocal :: MathExpr -> MathExpr
reciprocal = Frac one

-- Utility functions

bracketedShow :: Show a => a -> String
bracketedShow x = "(" ++ show x ++ ")"

dupe :: a -> (a, a)
dupe x = (x, x)

nthOrLast :: Int -> [a] -> a
nthOrLast n xs = last (drop (n - 1) xs)

hasZeroLit :: Foldable t => t MathExpr -> Bool
hasZeroLit = any f where
  f (IntLit 0) = True
  f _ = False

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = foldr foldFunc Nothing where
  foldFunc x@(Just _) _ = x
  foldFunc Nothing y = y

-- Derivatives

-- |Take derivative of an expression
der :: String -> MathExpr -> MathExpr
der _ (IntLit _) = zero
der dVar (Var var)
  | dVar == var = one
  | otherwise = zero
der dVar (Neg x) = (Neg . der dVar) x
der dVar (Sum es) = (Sum . map (der dVar)) es
der dVar (Prod es) = (Sum . concat . aux []) es where -- assumes commutativity of addition and multplication
  aux :: [MathExpr] -> [MathExpr] -> [[MathExpr]]
  aux _ [] = []
  aux prevs (h:ts) = (der dVar h : prevs ++ ts) : aux (h : prevs) ts
der dVar (Frac num den) = Frac
  ( Sum
    [ Prod [der dVar num, den]
    , Neg (Prod [num, der dVar den]) ] )
  ( Prod [den, den] )
der dVar (Exp x) = Prod
  [ x
  , Exp (Sum [x, negOne])
  , der dVar x ]
der dVar (Ln x) = Frac (der dVar x) x

-- Expression simplification

-- |Simplify the first expression in a list that can be simplified
trySimplifyExprList :: [MathExpr] -> Maybe [MathExpr] -- TODO - optimise with tail-recursion
trySimplifyExprList [] = Nothing
trySimplifyExprList (eh:ets) = case trySimplifyStep eh of
  Just eh' -> Just (eh' : ets)
  Nothing -> trySimplifyExprList ets >>= Just . (eh :)

-- |Try to simplify a subexpression of an expression
trySimplifyChildren :: MathExpr -> Maybe MathExpr
trySimplifyChildren (Neg e) = trySimplifyStep e >>= (return . Neg)
trySimplifyChildren (Sum es) = trySimplifyExprList es >>= Just . Sum
trySimplifyChildren (Prod es) = trySimplifyExprList es >>= Just . Prod
trySimplifyChildren (Frac e1 e2) = firstJust
  [ trySimplifyStep e1 >>= Just . flip Frac e2
  , trySimplifyStep e2 >>= Just . Frac e1 ]
trySimplifyChildren (Exp e) = trySimplifyStep e >>= (return . Exp)
trySimplifyChildren (Ln e) = trySimplifyStep e >>= (return . Ln)
trySimplifyChildren _ = Nothing

-- |Try to perform a pre-coded simplifying step on an expression
trySimplifyStep :: MathExpr -> Maybe MathExpr
trySimplifyStep e@(Sum _) = firstJust
  [ trySimplifyChildren e ] -- TODO - simplify 0s in sums, nested sums
trySimplifyStep e@(Prod _) = firstJust
  [ trySimplifyChildren e ] -- TODO - simplify 0s or 1s in products, nested products
-- Simplify e^(ln(x)), -(-(x))
trySimplifyStep e = trySimplifyChildren e

-- |The sequence of simplifying steps in the process of simplifying an expression
simplifyingSeq :: MathExpr -> [MathExpr]
simplifyingSeq e = e : unfoldr unfoldFunc e where
  unfoldFunc :: MathExpr -> Maybe (MathExpr, MathExpr)
  unfoldFunc e' = trySimplifyStep e' >>= Just . dupe

-- |Try to simplify the given expression by performing at most the number of simplification steps specified
simplifyAtMost :: Int -> MathExpr -> MathExpr
simplifyAtMost maxSteps = nthOrLast maxSteps . simplifyingSeq

-- |Try to simplify the expression as much as possible and return the result.
-- NOTE: there is no guarantee that this function will terminate:
-- depending on how the simplification is implemented, this function could end up running forever
simplifyFully :: MathExpr -> MathExpr
simplifyFully = last . simplifyingSeq
