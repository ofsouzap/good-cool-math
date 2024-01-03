module EquatingTests ( spec ) where

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  , shouldBe )
import Test.QuickCheck
  ( property )
import Utils
import GoodCoolMath ( MathExpr(..), (=~=) )

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
        \ (ShufflePair (xs, ShuffledNonEmpty ys)) -> Sum xs =~= Sum ys
      it "should equate products of the same list with same ordering" $ property $
        \ es -> Prod es =~= Prod es
      it "should equate products of the same list even when permuted" $ property $
        \ (ShufflePair (xs, ShuffledNonEmpty ys)) -> Prod xs =~= Prod ys
      it "should equate fractions with the same numerators and denominators" $ property $
        \ e1 e2 -> Frac e1 e2 =~= Frac e1 e2
      it "should equate exponentials with the same subexpression" $ property $
        \ e -> Exp e =~= Exp e
      it "should equate logarithms with the same subexpression" $ property $
        \ e -> Ln e =~= Ln e
