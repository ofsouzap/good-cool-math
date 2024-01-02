{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module GoodCoolMath
  ( MathExpr(..)
  , (=~=)
  , OrderedMathExpr(..)
  , MathExprLeaf(..)
  , unwrapExprLeaf
  , trySimplifyStep
  , simplifyAtMost
  , simplifyFully
  , (=->..<-=) ) where

import Data.List ( intercalate, unfoldr, sort )
import Test.QuickCheck
  ( Arbitrary
  , Gen
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

instance Show MathExpr where
  show (IntLit x) = show x
  show (Var vName) = vName
  show (Neg m) = "-" ++ bracketedShow m
  show (Sum m) = (intercalate "+" . map bracketedShow) m
  show (Prod m) = (intercalate "*" . map bracketedShow) m
  show (Frac num den) = bracketedShow num ++ "/" ++ bracketedShow den
  show (Exp x) = "e^" ++ bracketedShow x
  show (Ln x) = "ln" ++ bracketedShow x

arbitraryVarName :: Gen String
arbitraryVarName = pure <$> (elements . filter (`notElem` ['e', 'i'])) ['a'..'z']

instance Arbitrary MathExpr where
  arbitrary = do
    eIntLit <- arbitrary >>= return . IntLit
    eVar <- arbitraryVarName >>= return . Var
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

-- |Test if the two expressions are structurally equal.
-- This will account for equivalence properties of the constructors, eg. commutativity of addition (1 + 2 == 2 + 1),
-- but won't equate expressions of different constructors
(=~=) :: MathExpr -> MathExpr -> Bool
IntLit a =~= IntLit b = a == b
Var a =~= Var b = a == b
Neg e1 =~= Neg e2 = e1 =~= e2
Sum es1 =~= Sum es2 = exprListStructEqual
  ((map unwrapOrderedExpr . sort . map OrderedMathExpr) es1)
  ((map unwrapOrderedExpr . sort . map OrderedMathExpr) es2)
Prod es1 =~= Prod es2 = exprListStructEqual
  ((map unwrapOrderedExpr . sort . map OrderedMathExpr) es1)
  ((map unwrapOrderedExpr . sort . map OrderedMathExpr) es2)
Frac e11 e12 =~= Frac e21 e22 = e11 =~= e21 && e12 =~= e22
Exp e1 =~= Exp e2 = e1 =~= e2
Ln e1 =~= Ln e2 = e1 =~= e2
_ =~= _ = False

-- |Test if two lists of math expressions' corresponding elements satisfy =~= and the lists have the same number of elements
exprListStructEqual :: [MathExpr] -> [MathExpr] -> Bool
exprListStructEqual [] [] = True
exprListStructEqual [] (_:_) = False
exprListStructEqual (_:_) [] = False
exprListStructEqual (xh:xts) (yh:yts) = xh =~= yh && exprListStructEqual xts yts

---------------------------
-- Expression leaf nodes --
---------------------------

-- |A wrapper over the math expression data type whose `Arbitrary` implementation only creates leaf nodes
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

------------------------
-- Ordered expression --
------------------------

-- |Type for internal use to allow ordering of math expressions to optimise comparisons
-- NOTE: The ordering used is arbitrary and signifies nothing except a consistent way to sort math expressions.
-- Be careful when using this
newtype OrderedMathExpr = OrderedMathExpr MathExpr
  deriving ( Show )

unwrapOrderedExpr :: OrderedMathExpr -> MathExpr
unwrapOrderedExpr (OrderedMathExpr e') = e'

instance Eq OrderedMathExpr where
  OrderedMathExpr (IntLit a) == OrderedMathExpr (IntLit b) = a == b
  OrderedMathExpr (Var a) == OrderedMathExpr (Var b) = a == b
  OrderedMathExpr (Neg e1) == OrderedMathExpr (Neg e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  OrderedMathExpr (Sum es1) == OrderedMathExpr (Sum es2) = length es1 == length es2 && all (uncurry (==)) (zip (map OrderedMathExpr es1) (map OrderedMathExpr es2))
  OrderedMathExpr (Prod es1) == OrderedMathExpr (Prod es2) = length es1 == length es2 && all (uncurry (==)) (zip (map OrderedMathExpr es1) (map OrderedMathExpr es2))
  OrderedMathExpr (Frac e11 e12) == OrderedMathExpr (Frac e21 e22) = OrderedMathExpr e11 == OrderedMathExpr e21 && OrderedMathExpr e12 == OrderedMathExpr e22
  OrderedMathExpr (Exp e1) == OrderedMathExpr (Exp e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  OrderedMathExpr (Ln e1) == OrderedMathExpr (Ln e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  _ == _ = False

instance Ord OrderedMathExpr where
  -- With the same constructor

  OrderedMathExpr (IntLit a) <= OrderedMathExpr (IntLit b) = a <= b
  OrderedMathExpr (Var a) <= OrderedMathExpr (Var b) = a <= b
  OrderedMathExpr (Neg e1) <= OrderedMathExpr (Neg e2) = OrderedMathExpr e1 <= OrderedMathExpr e2
  OrderedMathExpr (Sum []) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Sum (_:_)) <= OrderedMathExpr (Sum []) = False
  OrderedMathExpr (Sum (xh:xts)) <= OrderedMathExpr (Sum (yh:yts)) = case compare (OrderedMathExpr xh) (OrderedMathExpr yh) of
    EQ -> OrderedMathExpr (Sum xts) <= OrderedMathExpr (Sum yts)
    LT -> True
    GT -> False
  OrderedMathExpr (Prod []) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Prod (_:_)) <= OrderedMathExpr (Prod []) = False
  OrderedMathExpr (Prod (xh:xts)) <= OrderedMathExpr (Prod (yh:yts)) = case compare (OrderedMathExpr xh) (OrderedMathExpr yh) of
    EQ -> OrderedMathExpr (Prod xts) <= OrderedMathExpr (Prod yts)
    LT -> True
    GT -> False
  OrderedMathExpr (Frac e11 e12) <= OrderedMathExpr (Frac e21 e22) = case compare (OrderedMathExpr e11) (OrderedMathExpr e21) of
    EQ -> OrderedMathExpr e12 <= OrderedMathExpr e22
    LT -> True
    GT -> False
  OrderedMathExpr (Exp e1) <= OrderedMathExpr (Exp e2) = OrderedMathExpr e1 <= OrderedMathExpr e2
  OrderedMathExpr (Ln e1) <= OrderedMathExpr (Ln e2) = OrderedMathExpr e1 <= OrderedMathExpr e2

  -- Otherwise, these orderings. I don't know how to do this without writing them all out :(

  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Var _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Neg _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (IntLit _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Var _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Var _) <= OrderedMathExpr (Neg _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Neg _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Sum _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Prod _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Exp _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Frac _ _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Ln _) <= OrderedMathExpr (IntLit _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Frac _ _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Exp _) = False

instance Arbitrary OrderedMathExpr where
  arbitrary = arbitrary >>= return . OrderedMathExpr

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
trySimplifyChildren (Neg e) = trySimplifyStep e >>= (return . Neg)
trySimplifyChildren (Sum es) = trySimplifyExprList es >>= Just . Sum
trySimplifyChildren (Prod es) = trySimplifyExprList es >>= Just . Prod
trySimplifyChildren (Frac e1 e2) = firstJust
  [ trySimplifyStep e1 >>= Just . flip Frac e2
  , trySimplifyStep e2 >>= Just . Frac e1 ]
trySimplifyChildren (Exp e) = trySimplifyStep e >>= (return . Exp)
trySimplifyChildren (Ln e) = trySimplifyStep e >>= (return . Ln)
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
