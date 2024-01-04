module GoodCoolMath.Shorthand where

import Data.List.NonEmpty
  ( NonEmpty((:|)) )
import GoodCoolMath.Expressions
  ( Const(..)
  , VarName(..)
  , MathExpr(..) )

-- | Shorthand for creating a variable by some name
var :: String -> MathExpr
var = Var . VarName

-- | An integer literal
int :: Int -> MathExpr
int = Const . IntLit

-- | The integer literal for zero
zero :: MathExpr
zero = int 0

-- | The integer literal for one
one :: MathExpr
one = int 1

-- | The integer literal for two
two :: MathExpr
two = int 2

-- | The negative of an expression
neg :: MathExpr -> MathExpr
neg = Neg

-- | Negative one
negOne :: MathExpr
negOne = neg one

-- | The fraction for a half (1/2)
half :: MathExpr
half = Frac one two

-- | Sum of two expressions
plus :: MathExpr -> MathExpr -> MathExpr
e1 `plus` e2 = Sum (e1 :| [e2])

-- | One expression minus another
-- a - b === a + (-b)
minus :: MathExpr -> MathExpr -> MathExpr
e1 `minus` e2 = e1 `plus` Neg e2

-- | The constant e
constE :: MathExpr
constE = Exp one

-- | The constant pi (3.14...)
constPi :: MathExpr
constPi = Const Pi

-- | Product of two expressions
times :: MathExpr -> MathExpr -> MathExpr
e1 `times` e2 = Prod (e1 :| [e2])

-- | One expression divided by another
dividedBy :: MathExpr -> MathExpr -> MathExpr
e1 `dividedBy` e2 = Frac e1 e2

-- | Expression squared
sqr :: MathExpr -> MathExpr
sqr e = e `times` e

-- | Expression cubed
cube :: MathExpr -> MathExpr
cube e = e `times` e `times` e

-- | A non-negative integer power of an expression expressed as the product of it with itself many times.
-- If a negative integer is provided, this function will throw an error
unsafeIntPowAsProd :: Int -> MathExpr -> MathExpr
unsafeIntPowAsProd n e
  | n < 0 = error "n must be non-negative"
  | n == 0 = one
  | n == 1 = e
  | otherwise = Prod (e :| [e | _ <- [2..n]])

-- | An expression to the power of another
-- b^n == exp(n * ln b)
pow :: MathExpr -> MathExpr -> MathExpr
pow b n = Exp (n `times` Ln b)

-- | Square root of an expression
-- sqrt(x) == x^(1/2)
sqrt :: MathExpr -> MathExpr
sqrt = flip pow half

reciprocal :: MathExpr -> MathExpr
reciprocal = Frac one

-- | n! for non-zero n is the product from i=1 to i=n of the value of i.
-- For n=0, the factorial is returned as a literal for 1.
-- NOTE: if a negative number is provided, this will return an error
unsafeFact :: Int -> MathExpr
unsafeFact 0 = one
unsafeFact 1 = one
unsafeFact n
  | n < 0 = error "n must be non-negative"
  | otherwise = Prod (int n :| [int i | i <- [1..(n-1)]])
