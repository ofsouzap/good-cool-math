module GoodCoolMath.Simplification
  ( trySimplifyStep
  , (=->..<-=)
  , simplifyAtMost
  , simplifyFully ) where

import GoodCoolMath.Expressions ( Const(..), MathExpr(..), (=~=), getIntLitVal, isIntLitOf )
import Data.List ( unfoldr )
import Data.List.NonEmpty
  ( NonEmpty((:|))
  , (<|)
  , nonEmpty )
import qualified Data.List.NonEmpty as NonEmpty
  ( map
  , appendList
  , last )
import Data.Maybe (fromMaybe)
import qualified Data.Monoid ( Sum(Sum), Product(Product) )
import Data.Monoid (Sum(getSum), Product (getProduct))
import Control.Arrow ( (***) )
import Data.Semigroup (Semigroup(sconcat))

-----------------------
-- Utility functions --
-----------------------

dupe :: a -> (a, a)
dupe x = (x, x)

nthOrLastNonEmpty :: Int -> NonEmpty a -> Maybe a
nthOrLastNonEmpty 0 (h:|_) = Just h
nthOrLastNonEmpty _ (h:|[]) = Just h
nthOrLastNonEmpty n (_:|(th:tts)) = nthOrLastNonEmpty (n-1) (th:|tts)

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = foldr foldFunc Nothing where
  foldFunc x@(Just _) _ = x
  foldFunc Nothing y = y

-- | Find all the combinations possible by taking one item from each list
allCombs :: [[a]] -> [[a]]
allCombs [] = [[]]
allCombs (hs:tss) = do
  h <- hs
  ts <- allCombs tss
  return (h : ts)

-- | Find all the combinations possible by taking one item from each non-empty list
allCombsNonEmpty :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
allCombsNonEmpty (hs:|[]) = do
  h <- hs
  return (h:|[])
allCombsNonEmpty (hs:|(th:tts)) = do
  h <- hs
  ts <- allCombsNonEmpty (th:|tts)
  return (h<|ts)

-- | Map a list but the mapping function should also return a boolean to signify whether a condition has been met.
-- If the condition is never met by the end of the mapping, return Nothing.
-- NOTE: output is reversed
mapOrFailReversed :: (Applicative m, Monoid (m b), Foldable t) => (a -> (Bool, b)) -> t a -> Maybe (m b)
mapOrFailReversed f = takeOutput . foldr foldFunc (False, mempty) where
  foldFunc x (state, acc) = case f x of
    (True, y) -> (True, pure y <> acc)
    (False, y) -> (state, pure y <> acc)
  takeOutput (False, _) = Nothing
  takeOutput (True, xs) = Just xs

-- | Map a non-empty list and keep track of how many values were counted with a True from the mapping function
mapAndCountNonEmpty :: (a -> (Bool, b)) -> NonEmpty a -> (Int, NonEmpty b) -- TODO - optimise with tail recursion (and rename to mention "reversing")
mapAndCountNonEmpty f (h:|[]) = case f h of
  (True, h') -> (1, h':|[])
  (False, h') -> (0, h':|[])
mapAndCountNonEmpty f (h:|th:tts) = let (n',ts') = mapAndCountNonEmpty f (th:|tts) in
  case f h of
    (True, h') -> (n'+1,h'<|ts')
    (False, h') -> (n',h'<|ts')

-- | Filter a list but return Nothing if no elements were removed.
-- NOTE: output is reversed
filterOrFailReversed :: (Applicative m, Monoid (m b), Foldable t) => (b -> Bool) -> t b -> Maybe (m b)
filterOrFailReversed f = takeOutput . foldr foldFunc (False, mempty) where
  foldFunc x (state, acc)
    | f x = (True, acc)
    | otherwise = (state, pure x <> acc)
  takeOutput (False, _) = Nothing
  takeOutput (True, xs) = Just xs

-- | Go through a foldable of elements and accumulate those that are mapped to a Just into a semigroup structure starting with the provided starting value
-- and return the number of elements accumulated, the semigroup result and the elements that weren't accumulated.
-- NOTE: output is reversed
semigroupPartitionAccumulateWithCountReversing :: (Semigroup sg, Foldable t) => (a -> Maybe sg) -> sg -> t a -> (Int, sg, [a])
semigroupPartitionAccumulateWithCountReversing partitioner start = foldr foldFunc (0, start, []) where
  foldFunc x (count, mAcc, xsAcc) = case partitioner x of
    Just x' -> (count+1, mAcc <> x', xsAcc)
    Nothing -> (count, mAcc, x : xsAcc)

-- | Go through a foldable of elements and accumulate those that are mapped to a Just into a monoid structure
-- and return the number of elements accumulated, the monoid result and the elements that weren't accumulated.
-- NOTE: output is reversed
monoidPartitionAccumulateWithCountReversing :: (Monoid m, Foldable t) => (a -> Maybe m) -> t a -> (Int, m, [a])
monoidPartitionAccumulateWithCountReversing = flip semigroupPartitionAccumulateWithCountReversing mempty

-------------------------------
-- Expression simplification --
-------------------------------

-- Usually, I'll aim to simplify expressions to a sum-of-products form

-- | Try to perform a pre-coded simplifying step on an expression
trySimplifyStep :: MathExpr -> Maybe MathExpr
trySimplifyStep (Neg (Neg e)) = Just e -- Negative of negative
trySimplifyStep e@(Sum es) = firstJust
  [ Sum <$> tryExpandSumSubsums es -- Flatten nested sums
  , Sum <$> tryCollectSumIntLits es -- Collect integer literal terms together
  , Sum <$> tryRemoveLiteralsOfReversing 0 es -- Remove 0s from sums
  , trySimplifyChildren e ] -- Otherwise, try simplify children
trySimplifyStep e@(Prod es) = firstJust
  [ Prod <$> tryExpandProdsSubprods es -- Flatten nested products
  , tryCollectProdNegs es -- Collect negative terms
  , Prod <$> tryExpandProdOfSums es -- "Multiply out" product-of-sums
  , Prod <$> tryCollectProdIntLits es -- Collect integer literal terms together
  , Prod <$> tryRemoveLiteralsOfReversing 1 es -- Remove 1s from products
  , trySimplifyChildren e ] -- Otherwise, try simplify children
  -- TODO - collect negatives in subexpressions, have 0 literal in product make whoole product 0
trySimplifyStep (Frac num den) = firstJust
  [ tryCollectFracNegs num den ] -- Collect negative terms
trySimplifyStep (Exp (Ln e)) = Just e -- Exponential of logarithm becomes the subexpression
trySimplifyStep (Exp (Sum es)) = (Just . Prod . NonEmpty.map Exp) es -- Exponential of sum becomes product of exponentials
trySimplifyStep e = trySimplifyChildren e

-- | The sequence of simplifying steps in the process of simplifying an expression
simplifyingSeq :: MathExpr -> NonEmpty MathExpr
simplifyingSeq e = e :| unfoldr unfoldFunc e where
  unfoldFunc :: MathExpr -> Maybe (MathExpr, MathExpr)
  unfoldFunc e' = trySimplifyStep e' >>= Just . dupe

-- | Try to simplify the given expression by performing at most the number of simplification steps specified
simplifyAtMost :: Int -> MathExpr -> MathExpr
simplifyAtMost maxSteps e = (fromMaybe e . nthOrLastNonEmpty maxSteps . simplifyingSeq) e

-- | Try to simplify the expression as much as possible and return the result.
-- NOTE: there is no guarantee that this function will terminate:
-- depending on how the simplification is implemented, this function could end up running forever
simplifyFully :: MathExpr -> MathExpr
simplifyFully = NonEmpty.last . simplifyingSeq

-- | Test if two expressions fully simplify to the same result
-- NOTE: this may not terminate if the simplification process doesn't terminate
(=->..<-=) :: MathExpr -> MathExpr -> Bool
e1 =->..<-= e2 = simplifyFully e1 =~= simplifyFully e2

-------------------------------------
-- Simplification helper functions --
-------------------------------------

-- | Simplify the first expression in a list that can be simplified
trySimplifyExprList :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
trySimplifyExprList = aux [] where
  aux :: [MathExpr] -> NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
  aux acc (eh:|ets) = case (trySimplifyStep eh, ets) of
    (Just eh', ets') -> Just (eh' :| (ets'<>acc))
    (Nothing, eth:etts) -> aux (eh:acc) (eth:|etts)
    (Nothing, []) -> Nothing

-- | Try to simplify a subexpression of an expression
trySimplifyChildren :: MathExpr -> Maybe MathExpr
trySimplifyChildren (Neg e) = Neg <$> trySimplifyStep e
trySimplifyChildren (Sum es) = trySimplifyExprList es >>= Just . Sum
trySimplifyChildren (Prod es) = trySimplifyExprList es >>= Just . Prod
trySimplifyChildren (Frac e1 e2) = firstJust
  [ trySimplifyStep e1 >>= Just . flip Frac e2
  , trySimplifyStep e2 >>= Just . Frac e1 ]
trySimplifyChildren (Exp e) = Exp <$> trySimplifyStep e
trySimplifyChildren (Ln e) = Ln <$> trySimplifyStep e
trySimplifyChildren _ = Nothing

tryExpandSumSubsums :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryExpandSumSubsums = fmap sconcat . (=<<) nonEmpty . mapOrFailReversed mapFunc where
  mapFunc :: MathExpr -> (Bool, NonEmpty MathExpr)
  mapFunc (Sum es) = (True, es)
  mapFunc e = (False, pure e)

tryExpandProdsSubprods :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryExpandProdsSubprods = fmap sconcat . (=<<) nonEmpty . mapOrFailReversed mapFunc where
  mapFunc :: MathExpr -> (Bool, NonEmpty MathExpr)
  mapFunc (Prod es) = (True, es)
  mapFunc e = (False, pure e)

-- | Remove from a list of math expressions any elements that are integer literals of the specified value.
-- Return Nothing if none found
tryRemoveLiteralsOfReversing :: Int -> NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryRemoveLiteralsOfReversing n = (=<<) nonEmpty . filterOrFailReversed (isIntLitOf n)

-- | Take terms and try to find all the integer literal terms and sum them together into a single integer literal term.
-- If less than two integer literal terms found, returns Nothing
tryCollectSumIntLits :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryCollectSumIntLits = aux . trySumIntLitsReversing where
  aux (n, s, xs)
    | n >= 2 = Just (NonEmpty.appendList ((pure . Const . IntLit . getSum) s) xs)
    | otherwise = Nothing

trySumIntLitsReversing :: Foldable t => t MathExpr -> (Int, Data.Monoid.Sum Int, [MathExpr])
trySumIntLitsReversing = monoidPartitionAccumulateWithCountReversing (fmap Data.Monoid.Sum . getIntLitVal)

-- | Take terms and try to find all the integer literal terms and multiply them together into a single integer literal term
-- If less than two integer literal terms found, returns Nothing
tryCollectProdIntLits :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryCollectProdIntLits = aux . tryProdIntLitsReversing where
  aux (n, p, xs)
    | n >= 2 = Just (NonEmpty.appendList ((pure . Const . IntLit . getProduct) p) xs)
    | otherwise = Nothing

tryProdIntLitsReversing :: Foldable t => t MathExpr -> (Int, Data.Monoid.Product Int, [MathExpr])
tryProdIntLitsReversing = monoidPartitionAccumulateWithCountReversing (fmap Data.Monoid.Product . getIntLitVal)

-- | Take sub-expressions of a product and "multiply out" any sum terms with each other
tryExpandProdOfSums :: NonEmpty MathExpr -> Maybe (NonEmpty MathExpr)
tryExpandProdOfSums = fmap (uncurry NonEmpty.appendList . (***) multiplyOutSumTerms id) . partitionSums

partitionSums :: NonEmpty MathExpr -> Maybe (NonEmpty (NonEmpty MathExpr), [MathExpr])
partitionSums = takeOutput . monoidPartitionAccumulateWithCountReversing partFunc where
  partFunc :: MathExpr -> Maybe [NonEmpty MathExpr]
  partFunc (Sum es) = Just (pure es)
  partFunc _ = Nothing
  takeOutput :: (Int, [NonEmpty MathExpr], [MathExpr]) -> Maybe (NonEmpty (NonEmpty MathExpr), [MathExpr])
  takeOutput (_, h:ts@(_:_), rems) = Just (NonEmpty.appendList (pure h) ts, rems)
  takeOutput (_, [_], _) = Nothing
  takeOutput (_, [], _) = Nothing

multiplyOutSumTerms :: NonEmpty (NonEmpty MathExpr) -> NonEmpty MathExpr
multiplyOutSumTerms = NonEmpty.map Prod . allCombsNonEmpty

-- | Try to collect negative terms in a non-empty list of a product's subexpressions.
-- If successful in finding any, returns the resulting product or negative of product term.
-- If no negatives found, returns Nothing.
tryCollectProdNegs :: NonEmpty MathExpr -> Maybe MathExpr
tryCollectProdNegs = fmap takeOutput . tryCheckNetNeg where
  takeOutput :: (Bool, NonEmpty MathExpr) -> MathExpr
  takeOutput (True, es) = (Neg . Prod) es
  takeOutput (False, es) = Prod es

-- | Take a sequence of math expressions and check if an odd number are Neg nodes.
-- If there are Neg nodes, a True means that the result should be negative.
-- If no Neg nodes are found in the list, return Nothing.
tryCheckNetNeg :: NonEmpty MathExpr -> Maybe (Bool, NonEmpty MathExpr)
tryCheckNetNeg = takeOutput . mapAndCountNonEmpty mapFunc where
  mapFunc :: MathExpr -> (Bool, MathExpr)
  mapFunc (Neg e') = (True, e')
  mapFunc e = (False, e)
  takeOutput :: (Int, NonEmpty MathExpr) -> Maybe (Bool, NonEmpty MathExpr)
  takeOutput (0, _) = Nothing
  takeOutput (n, es)
    | even n = Just (False, es)
    | otherwise = Just (True, es)

-- | Take the numerator and denominator of a fraction and try to find any negative terms and collect them.
-- If any are found, returns the fixed fraction term, otherwise returns Nothing
tryCollectFracNegs :: MathExpr -> MathExpr -> Maybe MathExpr
tryCollectFracNegs (Neg num') (Neg den') = Just (Frac num' den')
tryCollectFracNegs (Neg num') den = (Just . Neg) (Frac num' den)
tryCollectFracNegs num (Neg den') = (Just . Neg) (Frac num den')
tryCollectFracNegs _ _ = Nothing
