{-# LANGUAGE NoImplicitPrelude #-}
module GoodCoolMath.Examples
  ( gaussian ) where

import Prelude
  ( ($) )
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

-- | The hyperbolic sine function.
-- The parameter is a variable with name "x"
sinh :: MathExpr
sinh = let x = Var "x" in
  (Exp x `minus` Exp (neg x)) `dividedBy` two

-- | The hyperbolic cosine function.
-- The parameter is a variable with name "x"
cosh :: MathExpr
cosh = let x = Var "x" in
  (Exp x `plus` Exp (neg x)) `dividedBy` two

-- | The hyperbolic tangent function.
-- The parameter is a variable with name "x"
tanh :: MathExpr
tanh = sinh `dividedBy` cosh
