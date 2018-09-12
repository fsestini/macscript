module Utils where

import Data.Bifunctor (bimap)
import Data.List (uncons, partition, sortBy)
import Data.Function (on)
import Data.List.NonEmptyZipper (NonEmptyZipper(..))

nezFind :: (a -> Bool) -> [a] -> Maybe (NonEmptyZipper a)
nezFind p xs = do
  let l = takeWhile (not . p) xs
  (x, r) <- uncons (dropWhile (not . p) xs)
  pure (NonEmptyZipper l x r)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f &&& g) x = (f x, g x)

const2 :: c -> a -> b -> c
const2 x _ _ = x

maybeM :: Applicative m => (a -> m ()) -> Maybe a -> m ()
maybeM = maybe (pure ())

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = let a = xs !! i ; b = xs !! j in
  flip fmap (zip xs [0..]) $ \(x,k) -> case k of
  _ | k == i -> b
  _ | k == j -> a
  _ -> x

overSubstring :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
overSubstring p f ss =
  let ((ixs,l2), l1) = bimap unzip id (partition (p . snd) (zip [0 :: Int ..] ss))
  in fmap snd . sortBy (on compare fst) . (l1 ++) . zip (ixs ++ [length ss..]) $ f l2
