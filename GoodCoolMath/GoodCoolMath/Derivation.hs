module GoodCoolMath.Derivation
  ( der ) where

import Data.List.NonEmpty
  ( NonEmpty((:|))
  , (<|) )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import GoodCoolMath.Expressions ( VarName(..), MathExpr(..) )
import GoodCoolMath.Shorthand
  ( zero
  , one
  , times )

------------------------
-- Taking derivatives --
------------------------

-- | Take derivative of an expression
der :: VarName -> MathExpr -> MathExpr
der _ (Const _) = zero
der dVar (Var v)
  | dVar == v = one
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
der dVar e@(Exp x) = e `times` der dVar x
der dVar (Ln x) = Frac (der dVar x) x
