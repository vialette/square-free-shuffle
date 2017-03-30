import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import qualified Control.Arrow as Arrow

{-|
  The 'frequencies' function returns the frequencies of the elements of the input
  list.
-}
frequencies :: [Char] -> [(Int, Char)]
frequencies = fmap (Foldable.length Arrow.&&& List.head) . List.group . List.sort

{-|
  The 'evenParikh' function returns true iff each letter has an even number of
  occurrences.
-}
evenParikh :: [Char] -> Bool
evenParikh = Foldable.all (even . Tuple.fst) . frequencies

oddParikh :: [Char] -> Bool
oddParikh = not . evenParikh

{-|
  The 'balancedSplit' function cut a list into two balanced parts. In case the
  input list has odd length, the first part receives the extra element.
-}
balancedSplit :: [Char] -> ([Char], [Char])
balancedSplit xs = List.splitAt (List.length xs + 1 `div` 2) xs

{-|
  The 'square' function returns true if the input list is a square
  (i.e., the first half list is equal to the second half list).
-}
isPerfectSquare :: [Char] -> Bool
isPerfectSquare xs = (even $ List.length xs) && Tuple.fst s == Tuple.snd s
  where
    s = balancedSplit xs

isShuffleSquare :: [Char] -> Bool
isShuffleSquare xs
  | odd n              = False
  | oddParikh xs       = False
  | isPerfectSquare xs = True
  | otherwise          = isShuffleSquareAux n xs
  where
    n = List.length xs

isShuffleSquareAux :: Int -> [Char] -> Bool
isShuffleSquareAux n xs = not . Set.null $ Foldable.foldl f initSet (List.tail xs)
  where
    b       = n `div` 2
    initSet = Set.singleton [List.head xs]

    f :: Set.Set [Char] -> Char -> Set.Set [Char]
    f acc x = Set.fromList $ Foldable.concat [g ys | ys <- Set.toList acc]
      where
        g ys
          | x == List.last ys && m < b = [ys, ys ++ [x]]
          | x == List.last ys          = [ys]
          | m < b                      = [ys ++ [x]]
          | otherwise                  = []
          where
            m = List.length ys


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
