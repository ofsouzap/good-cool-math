module Utils where

import Data.List.NonEmpty
  ( NonEmpty((:|))
  , toList
  , fromList )
import qualified Data.List.NonEmpty as NonEmpty
  ( map )
import Test.Hspec
  ( shouldSatisfy
  , Expectation )
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , shuffle
  , suchThat, elements )
import GoodCoolMath
import GoodCoolMath.Shorthand

-- General

isSumTerm :: MathExpr -> Bool
isSumTerm (Sum _) = True
isSumTerm _ = False

shuffleNonEmpty :: NonEmpty a -> Gen (NonEmpty a)
shuffleNonEmpty = fmap fromList . (shuffle . toList)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    h <- arbitrary
    ts <- arbitrary
    return (h :| ts)

-- Tests

approxEq :: (Ord t, Num t) => t -> t -> t -> Bool
approxEq exp atol = (>) atol . abs . (-) exp

shouldApproxEq :: (Show t, Ord t, Num t) => t -> t -> t -> Expectation
shouldApproxEq exp atol = (`shouldSatisfy` approxEq exp atol)

isJustWhichApproxEq :: (Ord t, Num t) => t -> t -> Maybe t -> Bool
isJustWhichApproxEq exp atol = maybe False (approxEq exp atol)

shouldBeJustWhichApproxEq :: (Show t, Ord t, Num t) => t -> t -> Maybe t -> Expectation
shouldBeJustWhichApproxEq exp atol = (`shouldSatisfy` isJustWhichApproxEq exp atol)

maybesApproxEqWithTol :: (Ord t, Num t) => t -> Maybe t -> Maybe t -> Bool
maybesApproxEqWithTol atol Nothing Nothing = True
maybesApproxEqWithTol atol (Just x) (Just y) = approxEq y atol x
maybesApproxEqWithTol atol Nothing (Just _) = False
maybesApproxEqWithTol atol (Just _) Nothing = False

-- Shuffled and unshuffled non-empty list pairs

newtype ShuffledNonEmpty a = ShuffledNonEmpty (NonEmpty a)
  deriving ( Show )

newtype ShufflePair a = ShufflePair (NonEmpty a, ShuffledNonEmpty a)
  deriving ( Show )

instance Arbitrary a => Arbitrary (ShufflePair a) where
  arbitrary = do
    xs <- arbitrary
    ys <- shuffleNonEmpty xs
    (return . ShufflePair) (xs, ShuffledNonEmpty ys)

-- MathExpr that is a leaf node (doesn't hold in it any other expressions)

newtype MathExprLeaf = MathExprLeaf MathExpr

unwrapExprLeaf :: MathExprLeaf -> MathExpr
unwrapExprLeaf (MathExprLeaf e') = e'

instance Show MathExprLeaf where
  show (MathExprLeaf e') = show e'

instance Arbitrary MathExprLeaf where
  arbitrary = do
    eIntLit <- MathExprLeaf . int <$> arbitrary
    eVar <- MathExprLeaf . Var <$> arbitrary
    elements [ eIntLit, eVar ]

-- Leaf MathExpr that isn't IntLit 0

-- | A leaf expression. If it is an integer literal then the value must not be zero
newtype NotZeroIntLitMathExprLeaf = NotZeroIntLitMathExprLeaf MathExpr
  deriving ( Show )

instance Arbitrary NotZeroIntLitMathExprLeaf where
  arbitrary = NotZeroIntLitMathExprLeaf <$> suchThat (unwrapExprLeaf <$> arbitrary) (not . isIntLitOf 0)

-- Leaf MathExpr that isn't a negative IntLit

-- | A leaf expression. If it is an integer literal then the value must be positive
newtype IntLitOnlyPosMathExprLeaf = IntLitOnlyPosMathExprLeaf MathExpr
  deriving ( Show )

instance Arbitrary IntLitOnlyPosMathExprLeaf where
  arbitrary = IntLitOnlyPosMathExprLeaf <$> suchThat (unwrapExprLeaf <$> arbitrary) (maybe True (> 0) . getIntLitVal)

-- MathExpr that isn't IntLit 0

newtype NonZeroIntLitMathExpr = NonZeroIntLitMathExpr MathExpr
  deriving ( Show )

unwrapNonZeroIntLitMathExpr :: NonZeroIntLitMathExpr -> MathExpr
unwrapNonZeroIntLitMathExpr (NonZeroIntLitMathExpr e) = e

instance Arbitrary NonZeroIntLitMathExpr where
  arbitrary = NonZeroIntLitMathExpr <$> suchThat (arbitrary :: Gen MathExpr) (not . isIntLitOf 0)

-- MathExpr that isn't IntLit 1

newtype NonOneIntLitMathExpr = NonOneIntLitMathExpr MathExpr
  deriving ( Show )

unwrapNonOneIntLitMathExpr :: NonOneIntLitMathExpr -> MathExpr
unwrapNonOneIntLitMathExpr (NonOneIntLitMathExpr e) = e

instance Arbitrary NonOneIntLitMathExpr where
  arbitrary = NonOneIntLitMathExpr <$> suchThat (arbitrary :: Gen MathExpr) (not . isIntLitOf 1)

-- MathExpr list containing IntLit 0s paired with not containing them

newtype IntLitZeroNonEmpty = IntLitZeroNonEmpty (NonEmpty MathExpr)
  deriving ( Show )

unwrapIntLitZeroNonEmpty :: IntLitZeroNonEmpty -> NonEmpty MathExpr
unwrapIntLitZeroNonEmpty (IntLitZeroNonEmpty es) = es

instance Arbitrary IntLitZeroNonEmpty where
  arbitrary = IntLitZeroNonEmpty . NonEmpty.map (const zero) <$> (arbitrary :: Gen (NonEmpty MathExpr))

newtype WithWithoutIntLitZeroMathExprs = WithWithoutIntLitZeroMathExprs (NonEmpty MathExpr, NonEmpty MathExpr)
  deriving ( Show )

instance Arbitrary WithWithoutIntLitZeroMathExprs where
  arbitrary = do
    zeros <- (arbitrary :: Gen IntLitZeroNonEmpty)
    others <- (arbitrary :: Gen (NonEmpty NonZeroIntLitMathExpr))
    shuffledXs <- shuffleNonEmpty (unwrapIntLitZeroNonEmpty zeros <> NonEmpty.map unwrapNonZeroIntLitMathExpr others)
    shuffledYs <- shuffleNonEmpty (NonEmpty.map unwrapNonZeroIntLitMathExpr others)
    (return . WithWithoutIntLitZeroMathExprs) (shuffledXs, shuffledYs)

-- MathExpr list containing IntLit 0s paired with not containing them

newtype IntLitOneNonEmpty = IntLitOneNonEmpty (NonEmpty MathExpr)
  deriving ( Show )

unwrapIntLitOneNonEmpty :: IntLitOneNonEmpty -> NonEmpty MathExpr
unwrapIntLitOneNonEmpty (IntLitOneNonEmpty es) = es

instance Arbitrary IntLitOneNonEmpty where
  arbitrary = IntLitOneNonEmpty . NonEmpty.map (const one) <$> (arbitrary :: Gen (NonEmpty MathExpr))

newtype WithWithoutIntLitOneMathExprs = WithWithoutIntLitOneMathExprs (NonEmpty MathExpr, NonEmpty MathExpr)
  deriving ( Show )

instance Arbitrary WithWithoutIntLitOneMathExprs where
  arbitrary = do
    zeros <- (arbitrary :: Gen IntLitOneNonEmpty)
    others <- (arbitrary :: Gen (NonEmpty NonOneIntLitMathExpr))
    shuffledXs <- shuffleNonEmpty (unwrapIntLitOneNonEmpty zeros <> NonEmpty.map unwrapNonOneIntLitMathExpr others)
    shuffledYs <- shuffleNonEmpty (NonEmpty.map unwrapNonOneIntLitMathExpr others)
    return $ WithWithoutIntLitOneMathExprs (shuffledXs, shuffledYs)

-- A list of summed terms, a MathExpr list containing the sums and the list without the sums

newtype NonSumMathExpr = NonSumMathExpr MathExpr

unwrapNonSumMathExpr :: NonSumMathExpr -> MathExpr
unwrapNonSumMathExpr (NonSumMathExpr e) = e

instance Arbitrary NonSumMathExpr where
  arbitrary = NonSumMathExpr <$> suchThat (arbitrary :: Gen MathExpr) (not . isSumTerm)

newtype SumsAndWithWithout = SumsAndWithWithout ([NonEmpty MathExpr], [MathExpr], [MathExpr])
  deriving ( Show )

instance Arbitrary SumsAndWithWithout where
  arbitrary = do
    sumTerms <- (arbitrary :: Gen [NonEmpty MathExpr])
    let sums = map Sum sumTerms
    othersWrapped <- (arbitrary :: Gen [NonSumMathExpr])
    let others = map unwrapNonSumMathExpr othersWrapped
    shuffled <- shuffle (sums ++ others)
    (return . SumsAndWithWithout) (sumTerms, shuffled, others)
