module Simplification
  ( trySimplifyStep
  , simplifyAtMost
  , simplifyFully
  , (=->..<-=) ) where

import Expressions ( MathExpr(..), (=~=) )
import Data.List ( unfoldr )

----------------------
-- Shortcuts for me --
----------------------

zero, one, negOne :: MathExpr
zero = IntLit 0
one = IntLit 1
negOne = Neg one

reciprocal :: MathExpr -> MathExpr
reciprocal = Frac one

-----------------------
-- Utility functions --
-----------------------

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

-----------------
-- Derivatives --
-----------------

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

-------------------------------
-- Expression simplification --
-------------------------------

-- Usually, I'll aim to simplify expressions to a sum-of-products form

-- |Try to perform a pre-coded simplifying step on an expression
trySimplifyStep :: MathExpr -> Maybe MathExpr
trySimplifyStep (Neg (Neg e)) = Just e -- Negative of negative
trySimplifyStep (Sum []) = (Just . IntLit) 0 -- Empty sum uses sum monoid empty
trySimplifyStep e@(Sum es) = firstJust
  [ tryExpandSumSubsums es >>= Just . Sum
  , trySimplifyChildren e ] -- TODO - simplify 0s in sums, IntLits summed
trySimplifyStep (Prod []) = (Just . IntLit) 1 -- Empty product uses product monoid empty
trySimplifyStep e@(Prod es) = firstJust
  [ tryExpandProdsSubprods es >>= Just . Prod
  , trySimplifyChildren e ] -- TODO - simplify 0s or 1s in products, IntLits producted
trySimplifyStep (Exp (Ln e)) = Just e -- Exponential of logarithm becomes the subexpression
trySimplifyStep (Exp (Sum es)) = (Just . Prod . map Exp) es -- Exponential of sum becomes product of exponentials
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

-- |Test if two expressions fully simplify to the same result
-- NOTE: this may not terminate if the simplification process doesn't terminate
(=->..<-=) :: MathExpr -> MathExpr -> Bool
e1 =->..<-= e2 = simplifyFully e1 =~= simplifyFully e2

-------------------------------------
-- Simplification helper functions --
-------------------------------------

-- |Simplify the first expression in a list that can be simplified
trySimplifyExprList :: [MathExpr] -> Maybe [MathExpr] -- TODO - optimise with tail-recursion
trySimplifyExprList [] = Nothing
trySimplifyExprList (eh:ets) = case trySimplifyStep eh of
  Just eh' -> Just (eh' : ets)
  Nothing -> trySimplifyExprList ets >>= Just . (eh :)

-- |Try to simplify a subexpression of an expression
trySimplifyChildren :: MathExpr -> Maybe MathExpr
trySimplifyChildren (Neg e) = Neg <$> trySimplifyStep e
trySimplifyChildren (Sum es) = trySimplifyExprList es >>= Just . Sum
trySimplifyChildren (Prod es) = trySimplifyExprList es >>= Just . Prod
trySimplifyChildren (Frac e1 e2) = firstJust
  [ trySimplifyStep e1 >>= Just . flip Frac e2
  , trySimplifyStep e2 >>= Just . Frac e1 ]
trySimplifyChildren (Exp e) = Exp <$> trySimplifyStep e
trySimplifyChildren (Ln e) = Ln <$> trySimplifyStep e
trySimplifyChildren _ = Nothing

tryExpandSumSubsums :: [MathExpr] -> Maybe [MathExpr]
tryExpandSumSubsums = takeOutput . aux (False, []) where
  aux :: (Bool, [MathExpr]) -> [MathExpr] -> (Bool, [MathExpr])
  aux (valid, acc) [] = (valid, acc)
  aux (_, acc) (Sum es:ts) = aux (True, es ++ acc) ts
  aux (valid, acc) (h:ts) = aux (valid, h:acc) ts
  takeOutput :: (Bool, [MathExpr]) -> Maybe [MathExpr]
  takeOutput (False, _) = Nothing
  takeOutput (True, xs) = Just xs

tryExpandProdsSubprods :: [MathExpr] -> Maybe [MathExpr]
tryExpandProdsSubprods = takeOutput . aux (False, []) where
  aux :: (Bool, [MathExpr]) -> [MathExpr] -> (Bool, [MathExpr])
  aux (valid, acc) [] = (valid, acc)
  aux (_, acc) (Prod es:ts) = aux (True, es ++ acc) ts
  aux (valid, acc) (h:ts) = aux (valid, h:acc) ts
  takeOutput :: (Bool, [MathExpr]) -> Maybe [MathExpr]
  takeOutput (False, _) = Nothing
  takeOutput (True, xs) = Just xs

