module GoodCoolMath.Examples
  ( gaussian ) where

import GoodCoolMath.Expressions ( MathExpr(..) )
import GoodCoolMath.Shorthand

-- | The probability distribution function of the Gaussian distribution.
-- The mean is a variable with name "m", the standard deviation is the variable with name "s" and the value is the variable with name "x".
-- Pi is, for now, represented by a variable with the name "pi"
gaussian :: MathExpr
gaussian = let (x,mu,sigma,pi_) = (Var "x", Var "m", Var "s", Var "pi") in
  Exp (neg $ sqr (x `minus` mu) `dividedBy` (int 2 `times` sqr sigma))
  `dividedBy`
  GoodCoolMath.Shorthand.sqrt (two `times` pi_ `times` sqr sigma)
