module Expressions
  ( MathExpr(..)
  , (=~=)
  , OrderedMathExpr(..)
  , MathExprLeaf(..)
  , unwrapExprLeaf ) where

import Data.List ( intercalate, sort )
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , elements )

bracketedShow :: Show a => a -> String
bracketedShow x = "(" ++ show x ++ ")"

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
    eIntLit <- IntLit <$> arbitrary
    eVar <- Var <$> arbitraryVarName
    eNeg <- Neg <$> arbitrary
    eSum <- arbitrary
    eProd <- arbitrary
    eFrac <- uncurry Frac <$> arbitrary
    eExp <- Exp <$> arbitrary
    eLn <- Ln <$> arbitrary
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
    eIntLit <- MathExprLeaf . IntLit <$> arbitrary
    eVar <- MathExprLeaf . Var <$> arbitrary
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
  arbitrary = OrderedMathExpr <$> arbitrary
