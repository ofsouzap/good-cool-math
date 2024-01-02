module EquatingTests where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property
  , Arbitrary
  , arbitrary
  , shuffle )
import GoodCoolMath ( MathExpr(..), (=~=) )

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

spec :: Spec
spec = do
  describe "Equating" $ do
    describe "With the same types" $ do
      it "should equate all equal integer literals" $ property $
        \ n -> IntLit n =~= IntLit n
      it "should equate all equal variables" $ property $
        \ vName -> Var vName =~= Var vName
      it "should equate negatives of the same subexpression" $ property $
        \ e -> Neg e =~= Neg e
      it "should equate sums of the same list with same ordering" $ property $
        \ es -> Sum es =~= Sum es
      it "should equate sums of the same list even when permuted" $ property $
        \ (ShufflePair (xs, ShuffledList ys)) -> Sum xs =~= Sum ys
      it "should equate products of the same list with same ordering" $ property $
        \ es -> Prod es =~= Prod es
      it "should equate products of the same list even when permuted" $ property $
        \ (ShufflePair (xs, ShuffledList ys)) -> Prod xs =~= Prod ys
      it "should equate fractions with the same numerators and denominators" $ property $
        \ e1 e2 -> Frac e1 e2 =~= Frac e1 e2
      it "should equate exponentials with the same subexpression" $ property $
        \ e -> Exp e =~= Exp e
      it "should equate logarithms with the same subexpression" $ property $
        \ e -> Ln e =~= Ln e
