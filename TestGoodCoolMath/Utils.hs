module Utils where

import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , shuffle
  , suchThat )
import GoodCoolMath

isSumTerm :: MathExpr -> Bool
isSumTerm (Sum _) = True
isSumTerm _ = False

-- Shuffled and unshuffled list pairs

newtype ShuffledList a = ShuffledList [a]
  deriving ( Show )

newtype ShufflePair a = ShufflePair ([a], ShuffledList a)
  deriving ( Show )

instance Arbitrary a => Arbitrary (ShufflePair a) where
  arbitrary = do
    xs <- arbitrary
    ys <- shuffle xs
    (return . ShufflePair) (xs, ShuffledList ys)

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

newtype IntLitZeroList = IntLitZeroList [MathExpr]
  deriving ( Show )

unwrapIntLitZeroList :: IntLitZeroList -> [MathExpr]
unwrapIntLitZeroList (IntLitZeroList es) = es

instance Arbitrary IntLitZeroList where
  arbitrary = IntLitZeroList . map (const (IntLit 0)) <$> (arbitrary :: Gen [MathExpr])

newtype WithWithoutIntLitZeroMathExprs = WithWithoutIntLitZeroMathExprs ([MathExpr], [MathExpr])
  deriving ( Show )

instance Arbitrary WithWithoutIntLitZeroMathExprs where
  arbitrary = do
    zeros <- (arbitrary :: Gen IntLitZeroList)
    others <- (arbitrary :: Gen [NonZeroIntLitMathExpr])
    shuffledXs <- shuffle (unwrapIntLitZeroList zeros ++ map unwrapNonZeroIntLitMathExpr others)
    shuffledYs <- shuffle (map unwrapNonZeroIntLitMathExpr others)
    return $ WithWithoutIntLitZeroMathExprs (shuffledXs, shuffledYs)

-- MathExpr list containing IntLit 0s paired with not containing them

newtype IntLitOneList = IntLitOneList [MathExpr]
  deriving ( Show )

unwrapIntLitOneList :: IntLitOneList -> [MathExpr]
unwrapIntLitOneList (IntLitOneList es) = es

instance Arbitrary IntLitOneList where
  arbitrary = IntLitOneList . map (const (IntLit 1)) <$> (arbitrary :: Gen [MathExpr])

newtype WithWithoutIntLitOneMathExprs = WithWithoutIntLitOneMathExprs ([MathExpr], [MathExpr])
  deriving ( Show )

instance Arbitrary WithWithoutIntLitOneMathExprs where
  arbitrary = do
    zeros <- (arbitrary :: Gen IntLitOneList)
    others <- (arbitrary :: Gen [NonOneIntLitMathExpr])
    shuffledXs <- shuffle (unwrapIntLitOneList zeros ++ map unwrapNonOneIntLitMathExpr others)
    shuffledYs <- shuffle (map unwrapNonOneIntLitMathExpr others)
    return $ WithWithoutIntLitOneMathExprs (shuffledXs, shuffledYs)

-- A list of summed terms, a MathExpr list containing the sums and the list without the sums

newtype NonSumMathExpr = NonSumMathExpr MathExpr

unwrapNonSumMathExpr :: NonSumMathExpr -> MathExpr
unwrapNonSumMathExpr (NonSumMathExpr e) = e

instance Arbitrary NonSumMathExpr where
  arbitrary = NonSumMathExpr <$> suchThat (arbitrary :: Gen MathExpr) (not . isSumTerm)

newtype SumsAndWithWithout = SumsAndWithWithout ([[MathExpr]], [MathExpr], [MathExpr])
  deriving ( Show )

instance Arbitrary SumsAndWithWithout where
  arbitrary = do
    sumTerms <- (arbitrary :: Gen [[MathExpr]])
    let sums = map Sum sumTerms
    othersWrapped <- (arbitrary :: Gen [NonSumMathExpr])
    let others = map unwrapNonSumMathExpr othersWrapped
    shuffled <- shuffle (sums ++ others)
    (return . SumsAndWithWithout) (sumTerms, shuffled, others)
