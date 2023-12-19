module GoodCoolMath
  ( MathExpr
  , simplifyStep ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Can the below be used for something later perhaps? Make it more general maybe???
-- class Math a where
--   neg :: a -> a
--   sumIdentity :: a
--   sumOne :: a -> a -> a
--   sum :: Foldable t => t a -> a
--   sum = foldr sumOne sumIdentity
--   prodIdentity :: a
--   prodOne :: a -> a -> a
--   prod :: Foldable t => t a -> a
--   prod = foldr prodOne sumIdentity

data MathExpr =
    IntLit Int
  | Var String
  | Neg MathExpr
  | Sum [MathExpr]
  | Prod [MathExpr]
  | Frac MathExpr MathExpr
  | Exp MathExpr
  | Ln MathExpr

instance Show MathExpr where
  show (IntLit x) = show x
  show (Var vName) = vName
  show (Neg m) = "-" ++ show m
  show (Sum m) = (intercalate "+" . map show) m
  show (Prod m) = (intercalate "*" . map show) m
  show (Frac num den) = show num ++ "/" ++ show den
  show (Exp x) = "e^" ++ show x
  show (Ln x) = "ln(" ++ show x ++ ")"


-- Shortcuts for me

zero, one, negOne :: MathExpr
zero = IntLit 0
one = IntLit 1
negOne = Neg one

reciprocal :: MathExpr -> MathExpr
reciprocal = Frac one

-- |Take derivative of an expression
der :: String -> MathExpr -> MathExpr
der _ (IntLit _) = zero
der dVar (Var var)
  | dVar == var = one
  | otherwise = zero
der dVar (Neg x) = (Neg . der dVar) x
der dVar (Sum xs) = (Sum . map (der dVar)) xs
der dVar (Prod xs) = (Sum . concat . aux []) xs where -- assumes commutativity of addition and multplication
  aux :: [MathExpr] -> [MathExpr] -> [[MathExpr]]
  aux _ [] = []
  aux prevs (h:ts) = (der dVar h : prevs ++ ts) : aux (h : prevs) ts
der dVar (Frac num den) = Frac (Sum [Prod [der dVar num, den], Neg (Prod [num, der dVar den])]) (Prod [den, den])
der dVar (Exp x) = Prod [x, Exp (Sum [x, negOne]), der dVar x]
der dVar (Ln x) = Frac (der dVar x) x

-- |Try to perform a pre-coded simplifying step on an expression
simplifyStep :: MathExpr -> Maybe MathExpr
simplifyStep (Sum xs) = (Just . Sum . filter isNonZeroLit . map attemptSimplify) xs where -- A sum with 0 in it
  isNonZeroLit (IntLit 0) = True
  isNonZeroLit _ = False
  attemptSimplify e = fromMaybe e (simplifyStep e)
simplifyStep x@(Prod xs) = if hasZeroLit xs then Just zero else Just x where -- A product with 0 in it
  hasZeroLit :: [MathExpr] -> Bool
  hasZeroLit [] = False
  hasZeroLit (IntLit 0:_) = True
  hasZeroLit (_:ts) = hasZeroLit ts
-- TODO - simplify product with 1 in it
-- TODO - more simplifying... (eg. e^(ln(x)), nested sums/products, etc.)
simplifyStep _ = Nothing
