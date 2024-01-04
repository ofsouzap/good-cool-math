module GoodCoolMath.Expressions
  ( Const(..)
  , VarName(..)
  , MathExpr(..)
  , (=~=)
  , OrderedConst(..)
  , OrderedMathExpr(..)
  , isIntLitWhere
  , isIntLitOf
  , isIntLit
  , getIntLitVal ) where

import Data.List.NonEmpty
  ( NonEmpty((:|))
  , intersperse )
import qualified Data.List.NonEmpty as NonEmpty
  ( map
  , sort
  , toList
  , zip )
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , elements )
import Data.Semigroup (Semigroup(sconcat))

bracketedShow :: Show a => a -> String
bracketedShow x = "(" ++ show x ++ ")"

---------------
-- Constants --
---------------

-- | A constant value
data Const =
    IntLit Int
  | NamedConst String
  | UnnamedConst
  | Pi
  deriving ( Eq )

instance Show Const where
  show (IntLit n) = show n
  show (NamedConst s) = show s
  show UnnamedConst = "const"
  show Pi = "π"

instance Arbitrary Const where
  arbitrary = do
    n <- (arbitrary :: Gen Int)
    s <- (arbitrary :: Gen String)
    elements
      [ IntLit n
      , NamedConst s
      , UnnamedConst
      , Pi ]

--------------------
-- Variable Names --
--------------------

newtype VarName = VarName String
  deriving ( Eq, Ord )

instance Show VarName where
  show (VarName x) = show x

instance Arbitrary VarName where
  arbitrary = VarName <$> (arbitrary :: Gen String)

----------------------
-- Math Expressions --
----------------------

data MathExpr =
    Const Const
  | Var VarName
  | Neg MathExpr
  | Sum (NonEmpty MathExpr)
  | Prod (NonEmpty MathExpr)
  | Frac MathExpr MathExpr
  | Exp MathExpr
  | Ln MathExpr

instance Show MathExpr where
  show (Const x) = show x
  show (Var (VarName vName)) = vName
  show (Neg m) = "-" ++ bracketedShow m
  show (Sum m) = (sconcat . intersperse "+" . NonEmpty.map bracketedShow) m
  show (Prod m) = (sconcat . intersperse "*"  . NonEmpty.map bracketedShow) m
  show (Frac num den) = bracketedShow num ++ "/" ++ bracketedShow den
  show (Exp x) = "e^" ++ bracketedShow x
  show (Ln x) = "ln" ++ bracketedShow x

arbitraryVarName :: Gen VarName
arbitraryVarName = VarName . pure <$> (elements . filter (`notElem` ['e', 'i'])) ['a'..'z']

instance Arbitrary MathExpr where
  arbitrary = do
    eConst <- Const <$> (arbitrary :: Gen Const)
    eVar <- Var <$> arbitraryVarName
    eNeg <- Neg <$> arbitrary
    eSum <- arbitrary
    eProd <- arbitrary
    eFrac <- uncurry Frac <$> arbitrary
    eExp <- Exp <$> arbitrary
    eLn <- Ln <$> arbitrary
    elements
      [ eConst
      , eVar
      , eNeg
      , eSum
      , eProd
      , eFrac
      , eExp
      , eLn ]

-- | Test if the two expressions are structurally equal.
-- This will account for equivalence properties of the constructors, eg. commutativity of addition (1 + 2 == 2 + 1),
-- but won't equate expressions of different constructors
(=~=) :: MathExpr -> MathExpr -> Bool
Const a =~= Const b = a == b
Var (VarName a) =~= Var (VarName b) = a == b
Neg e1 =~= Neg e2 = e1 =~= e2
Sum es1 =~= Sum es2 = exprListStructEqual
  ((NonEmpty.toList . NonEmpty.map unwrapOrderedExpr . NonEmpty.sort . NonEmpty.map OrderedMathExpr) es1)
  ((NonEmpty.toList . NonEmpty.map unwrapOrderedExpr . NonEmpty.sort . NonEmpty.map OrderedMathExpr) es2)
Prod es1 =~= Prod es2 = exprListStructEqual
  ((NonEmpty.toList . NonEmpty.map unwrapOrderedExpr . NonEmpty.sort . NonEmpty.map OrderedMathExpr) es1)
  ((NonEmpty.toList . NonEmpty.map unwrapOrderedExpr . NonEmpty.sort . NonEmpty.map OrderedMathExpr) es2)
Frac e11 e12 =~= Frac e21 e22 = e11 =~= e21 && e12 =~= e22
Exp e1 =~= Exp e2 = e1 =~= e2
Ln e1 =~= Ln e2 = e1 =~= e2
_ =~= _ = False

-- | Test if two lists of math expressions' corresponding elements satisfy =~= and the lists have the same number of elements
exprListStructEqual :: [MathExpr] -> [MathExpr] -> Bool
exprListStructEqual [] [] = True
exprListStructEqual [] (_:_) = False
exprListStructEqual (_:_) [] = False
exprListStructEqual (xh:xts) (yh:yts) = xh =~= yh && exprListStructEqual xts yts

------------------------
-- Ordered expression --
------------------------

-- | Type for internal use to allow ordering of constants to optimise comparisons
-- NOTE: The ordering used is arbitrary and signifies nothing except a consistent way to sort constants.
-- Be careful when using this
newtype OrderedConst = OrderedConst Const
  deriving ( Show, Eq )

instance Ord OrderedConst where
  OrderedConst (IntLit a) <= OrderedConst (IntLit b) = a <= b
  OrderedConst (IntLit _) <= OrderedConst (NamedConst _) = True
  OrderedConst (IntLit _) <= OrderedConst UnnamedConst = True
  OrderedConst (IntLit _) <= OrderedConst Pi = True

  OrderedConst (NamedConst _) <= OrderedConst (IntLit _) = False
  OrderedConst (NamedConst a) <= OrderedConst (NamedConst b) = a <= b
  OrderedConst (NamedConst _) <= OrderedConst UnnamedConst = True
  OrderedConst (NamedConst _) <= OrderedConst Pi = True

  OrderedConst UnnamedConst <= OrderedConst (IntLit _) = False
  OrderedConst UnnamedConst <= OrderedConst (NamedConst _) = False
  OrderedConst UnnamedConst <= OrderedConst UnnamedConst = True
  OrderedConst UnnamedConst <= OrderedConst Pi = True

  OrderedConst Pi <= OrderedConst (IntLit _) = False
  OrderedConst Pi <= OrderedConst (NamedConst _) = False
  OrderedConst Pi <= OrderedConst UnnamedConst = False
  OrderedConst Pi <= OrderedConst Pi = True

instance Arbitrary OrderedConst where
  arbitrary = OrderedConst <$> arbitrary

-- | Type for internal use to allow ordering of math expressions to optimise comparisons
-- NOTE: The ordering used is arbitrary and signifies nothing except a consistent way to sort math expressions.
-- Be careful when using this
newtype OrderedMathExpr = OrderedMathExpr MathExpr
  deriving ( Show )

unwrapOrderedExpr :: OrderedMathExpr -> MathExpr
unwrapOrderedExpr (OrderedMathExpr e') = e'

instance Eq OrderedMathExpr where
  OrderedMathExpr (Const a) == OrderedMathExpr (Const b) = OrderedConst a == OrderedConst b
  OrderedMathExpr (Var (VarName a)) == OrderedMathExpr (Var (VarName b)) = a == b
  OrderedMathExpr (Neg e1) == OrderedMathExpr (Neg e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  OrderedMathExpr (Sum es1) == OrderedMathExpr (Sum es2) =
    length es1 == length es2
    && all (uncurry (==)) (NonEmpty.zip (NonEmpty.map OrderedMathExpr es1) (NonEmpty.map OrderedMathExpr es2))
  OrderedMathExpr (Prod es1) == OrderedMathExpr (Prod es2) =
    length es1 == length es2
    && all (uncurry (==)) (NonEmpty.zip (NonEmpty.map OrderedMathExpr es1) (NonEmpty.map OrderedMathExpr es2))
  OrderedMathExpr (Frac e11 e12) == OrderedMathExpr (Frac e21 e22) = OrderedMathExpr e11 == OrderedMathExpr e21 && OrderedMathExpr e12 == OrderedMathExpr e22
  OrderedMathExpr (Exp e1) == OrderedMathExpr (Exp e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  OrderedMathExpr (Ln e1) == OrderedMathExpr (Ln e2) = OrderedMathExpr e1 == OrderedMathExpr e2
  _ == _ = False

instance Ord OrderedMathExpr where
  -- With the same constructor

  OrderedMathExpr (Const a) <= OrderedMathExpr (Const b) = OrderedConst a <= OrderedConst b
  OrderedMathExpr (Var (VarName a)) <= OrderedMathExpr (Var (VarName b)) = a <= b
  OrderedMathExpr (Neg e1) <= OrderedMathExpr (Neg e2) = OrderedMathExpr e1 <= OrderedMathExpr e2
  OrderedMathExpr (Sum (_:|[])) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Sum (_:|(_:_))) <= OrderedMathExpr (Sum (_:|[])) = False
  OrderedMathExpr (Sum (xh:|(xth:xtts))) <= OrderedMathExpr (Sum (yh:|(yth:ytts))) = case compare (OrderedMathExpr xh) (OrderedMathExpr yh) of
    EQ -> OrderedMathExpr (Sum (xth:|xtts)) <= OrderedMathExpr (Sum (yth:|ytts))
    LT -> True
    GT -> False
  OrderedMathExpr (Prod (_:|[])) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Prod (_:|(_:_))) <= OrderedMathExpr (Prod (_:|[])) = False
  OrderedMathExpr (Prod (xh:|(xth:xtts))) <= OrderedMathExpr (Prod (yh:|(yth:ytts))) = case compare (OrderedMathExpr xh) (OrderedMathExpr yh) of
    EQ -> OrderedMathExpr (Prod (xth:|xtts)) <= OrderedMathExpr (Prod (yth:|ytts))
    LT -> True
    GT -> False
  OrderedMathExpr (Frac e11 e12) <= OrderedMathExpr (Frac e21 e22) = case compare (OrderedMathExpr e11) (OrderedMathExpr e21) of
    EQ -> OrderedMathExpr e12 <= OrderedMathExpr e22
    LT -> True
    GT -> False
  OrderedMathExpr (Exp e1) <= OrderedMathExpr (Exp e2) = OrderedMathExpr e1 <= OrderedMathExpr e2
  OrderedMathExpr (Ln e1) <= OrderedMathExpr (Ln e2) = OrderedMathExpr e1 <= OrderedMathExpr e2

  -- Otherwise, these orderings. I don't know how to do this without writing them all out :(

  OrderedMathExpr (Const _) <= OrderedMathExpr (Var _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Neg _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Const _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Var _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Var _) <= OrderedMathExpr (Neg _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Var _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Neg _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Sum _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Neg _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Sum _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Prod _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Sum _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Prod _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Frac _ _) = True
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Prod _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Exp _) = True
  OrderedMathExpr (Frac _ _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Exp _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Frac _ _) = False
  OrderedMathExpr (Exp _) <= OrderedMathExpr (Ln _) = True

  OrderedMathExpr (Ln _) <= OrderedMathExpr (Const _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Var _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Neg _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Sum _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Prod _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Frac _ _) = False
  OrderedMathExpr (Ln _) <= OrderedMathExpr (Exp _) = False

instance Arbitrary OrderedMathExpr where
  arbitrary = OrderedMathExpr <$> arbitrary

-----------------------
-- Utility Functions --
-----------------------

isIntLitWhere :: (Int -> Bool) -> MathExpr -> Bool
isIntLitWhere f (Const (IntLit x)) = f x
isIntLitWhere _ _ = False

isIntLit :: MathExpr -> Bool
isIntLit = isIntLitWhere (const True)

isIntLitOf :: Int -> MathExpr -> Bool
isIntLitOf n = isIntLitWhere (== n)

hasZeroLit :: Foldable t => t MathExpr -> Bool
hasZeroLit = any (isIntLitOf 0)

getIntLitVal :: MathExpr -> Maybe Int
getIntLitVal (Const (IntLit x)) = Just x
getIntLitVal _ = Nothing
