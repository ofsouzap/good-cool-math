module Derivation
  ( der ) where

import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import Expressions ( MathExpr(..) )

----------------------
-- Shortcuts for me --
----------------------

zero, one, negOne :: MathExpr
zero = IntLit 0
one = IntLit 1
negOne = Neg one

reciprocal :: MathExpr -> MathExpr
reciprocal = Frac one

------------------------
-- Taking derivatives --
------------------------

-- | Take derivative of an expression
der :: String -> MathExpr -> MathExpr
der _ (IntLit _) = zero
der dVar (Var var)
  | dVar == var = one
  | otherwise = zero
der dVar (Neg x) = (Neg . der dVar) x
der dVar (Sum es) = (Sum . NonEmpty.map (der dVar)) es
der _ (Prod _) = undefined -- TODO
der _ (Frac _ _) = undefined -- TODO
der _ (Exp _) = undefined -- TODO
der dVar (Ln x) = Frac (der dVar x) x
