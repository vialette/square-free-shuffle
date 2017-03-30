{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List                 as L
import qualified Data.Set                  as S
import qualified Data.List.Split.Internals as Split
import qualified Data.Tuple                as T
import qualified Data.Foldable             as F
import qualified Data.Map.Strict           as M
import qualified Data.Traversable          as Traversable
import qualified Control.Monad             as Monad
import qualified Control.Arrow             as Arrow
import Data.Maybe

contiguous k = L.filter (\l -> L.length l == k) . L.concat . L.map L.inits . L.tails

{-|
  The 'evenLength' function returns true for even length lists, and false
  otherwise.
-}
evenLength :: [a] -> Bool
evenLength = even . L.length

oddLength = not . evenLength


{-|
  The 'frequencies' function returns the frequencies of the elements of the input
  list.
-}
frequencies :: Ord a => [a] -> [(Int, a)]
frequencies = L.map (L.length Arrow.&&& L.head) . L.group . L.sort

{-|
  The 'evenParikh' function returns true iff each letter has an even number of
  occurrences.
-}
evenParikh :: Ord a => [a] -> Bool
evenParikh = F.all (even . T.fst) . frequencies

{-|
  The 'balancedSplit' function cut a list into two balanced parts. In case the
  input list has odd length, the first part receives the extra element.
-}
balancedSplit :: [a] -> ([a], [a])
balancedSplit xs = L.splitAt (L.length xs + 1 `div` 2) xs

{-|
  The 'square' function returns true if the input list is a square
  (i.e., the first half list is equal to the second half list).
-}
isPerfectSquare :: Eq a => [a] -> Bool
isPerfectSquare xs = evenLength xs && T.fst s == T.snd s
  where
    s = balancedSplit xs

balancedSplits xs
  | oddLength xs = []
  | otherwise    = [(yz, xs L.\\ ys) | ys <- xs `choose` k]
  where
    k = (L.length xs) `div` 2


isSquare = not . F.any (\ balancedSplit -> balancedSplit.fst == balancedSplit.snd) . balancedSplits

isConsecutiveSquareFreePermutation xs
  | L.length xs <= 4 = False
  | otherwise        = F.none [xs' | k <- [6, 8..n], xs' <- xs `choose` k, not (isSquare xs')]
  where
    n = (L.length xs) `div` 2

consecutiveSquareFreePermutations n = L.filter isConsecutiveSquareFreePermutation ps
  where
    ps = L.permutations [1..n]

main :: IO ()
main = print (squareFreePermutations 5)
