module GoodCoolMath.Derivation
  ( der ) where

import Data.List.NonEmpty
  ( NonEmpty((:|))
  , (<|) )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import GoodCoolMath.Expressions ( MathExpr(..) )
import GoodCoolMath.Shorthand

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
der dVar (Prod es) = (Sum . NonEmpty.map Prod . aux []) es where
  aux :: [MathExpr] -> NonEmpty MathExpr -> NonEmpty (NonEmpty MathExpr) -- TODO - optimise with tail recursion
  aux prevs (h:|[]) = pure (der dVar h :| prevs)
  aux prevs (h:|ts@(th:tts)) = (der dVar h :| prevs ++ ts) <| aux (h:prevs) (th:|tts)
der dVar (Frac num den) = Frac
  ( Sum
      ( Prod (der dVar num:|[den]) :| -- d/dx num * den
      [ Neg (Prod (num:|[der dVar den])) ] ) ) -- -num * d/dx den
  ( Prod (den:|[ den ]) ) -- den^2
der dVar (Exp x) = Prod
  ( x :|
  [ Exp (Sum (x:|[negOne]))
  , der dVar x ] )
der dVar (Ln x) = Frac (der dVar x) x
