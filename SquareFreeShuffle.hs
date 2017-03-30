
import Prelude hiding (Word)
import qualified Data.Tuple    as Tuple
import qualified Data.List     as List
import qualified Data.Set      as Set
import qualified Data.Array    as Array
import qualified Data.Foldable as Foldable
import qualified Control.Arrow as Arrow

type Letter = Int
type Word = [Letter]

{-|
  The 'frequencies' function returns the frequencies of the elements of the input
  list.
-}
frequencies :: Word -> [(Int, Int)]
frequencies = fmap (Foldable.length Arrow.&&& List.head) . List.group . List.sort

{-|
  The 'evenParikh' function returns true iff each letter has an even number of
  occurrences.
-}
evenParikh :: Word -> Bool
evenParikh = Foldable.all (even . Tuple.fst) . frequencies

oddParikh :: Word -> Bool
oddParikh = not . evenParikh

{-|
  The 'balancedSplit' function cut a list into two balanced parts. In case the
  input list has odd length, the first part receives the extra element.
-}
balancedSplit :: Word -> (Word, Word)
balancedSplit xs = List.splitAt (List.length xs + 1 `div` 2) xs

{-|
  The 'square' function returns true if the input list is a square
  (i.e., the first half list is equal to the second half list).
-}
isPerfectSquare :: Word -> Bool
isPerfectSquare xs = (even $ List.length xs) && Tuple.fst s == Tuple.snd s
  where
    s = balancedSplit xs

--isShuffleSquare :: Word -> Bool
isShuffleSquare xs
  | odd n              = False
  | oddParikh xs       = False
  | isPerfectSquare xs = True
  | otherwise          = not . List.null $ shuffleSquares xs
  where
    n = List.length xs

shuffleSquares :: Word -> [Word]
shuffleSquares []  = [[]]
shuffleSquares [x] = []
shuffleSquares xs  = shuffleSquaresAux 0 (n `div` 2) initialArray (List.tail xs)
  where
    n            = List.length xs
    initialArray = Array.listArray (0, 0) [Set.fromList [[List.head xs]]]

shuffleSquaresAux :: Array.Ix i => Int -> i -> Array.Array i (Set.Set Word) -> Word -> [Word]
shuffleSquaresAux _ b array []       = Set.toList $ array Array.! b
shuffleSquaresAux i b array (x : xs) = shuffleSquaresAux (i+1) b array' xs
  where
    array' = shuffleSquaresAux' b array x

shuffleSquaresAux' :: Array.Ix i => Int -> i -> Array.Array i (Set.Set Word) -> Letter -> Array.Array i (Set.Set Word)
shuffleSquaresAux' i b array x = Array.listArray (0, i `div` 2) [f i | i <- [0..i `div` 2]]
  where
    f j =

    --     prev_array    = [None] * n
    --     current_array = [None] * n
    --     current_array[0] = set([u[0]])
    --
    --     for i in range(2, n + 1):
    --         prev_array = current_array
    --         current_array = [None] * n
    --         for j in range(max(0, i - n//2), i//2 + 1):
    --             current_array[j] = set()
    --             if j > 0:
    --                 for v in prev_array[j - 1]:
    --                     if v[j - 1] == u[i - 1]:
    --                         current_array[j].add(v)
    --             if j <= min((i-1)//2, n//2):
    --                 for v in prev_array[j]:
    --                     current_array[j].add(v + u[i-1])
    --
    --     return len(current_array[n//2]) > 0
