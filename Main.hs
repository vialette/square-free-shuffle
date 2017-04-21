module Main

where

  import System.IO
  import qualified Data.Tuple      as Tuple
  import qualified Data.List       as List
  import qualified Data.Set        as Set
  import qualified Data.Foldable   as Foldable
  import qualified Data.Array      as Array
  import qualified Data.Monoid     as Monoid
  import qualified Data.Char       as Char
  import qualified Data.Map.Strict as Map
  import qualified Control.Arrow   as Arrow

  newtype Alphabet = Alphabet { getSize :: Int }

  type Dictionary = Set.Set String

  mkAlphabet :: Int -> Alphabet
  mkAlphabet n
    | n <= 0    = Alphabet { getSize = 0 }
    | otherwise = Alphabet { getSize = n }

  firstLetter :: Char
  firstLetter = 'a'

  lastLetter :: Alphabet -> Char
  lastLetter Alphabet { getSize = s } = Char.chr $ Char.ord 'a' + (s-1)

  nextLetter :: Alphabet -> Char -> Maybe Char
  nextLetter alphabet c
    | c < firstLetter          = Nothing
    | c > lastLetter alphabet  = Nothing
    | c == lastLetter alphabet = Nothing
    | otherwise                = Just $ Char.chr ((1 + Char.ord c - Char.ord 'a') + Char.ord 'a')

  evenLengthPrefixes :: String -> [String]
  evenLengthPrefixes [] = []
  evenLengthPrefixes xs = fmap Tuple.snd . List.filter (even . Tuple.fst) . List.zip [1..] . List.tail $ List.inits xs

  {-|
    The 'frequencies' function returns the frequencies of the elements of the input
    list.
  -}
  frequencies :: String -> [(Int, Char)]
  frequencies = fmap (Foldable.length Arrow.&&& List.head) . List.group . List.sort

  {-|
    The 'evenParikh' function returns true iff each letter has an even number of
    occurrences.
  -}
  evenParikh :: String -> Bool
  evenParikh = Foldable.all (even . Tuple.fst) . frequencies

  {-|
    The 'oddParikh' function returns true iff each letter does not have an even number of
    occurrences.
  -}
  oddParikh :: String -> Bool
  oddParikh = not . evenParikh

  {-|
    The 'balancedSplit' function cut a list into two balanced parts. In case the
    input list has odd length, the first part receives the extra element.
  -}
  balancedSplit :: String -> (String, String)
  balancedSplit xs = List.splitAt (List.length xs `div` 2) xs

  {-|
    The 'square' function returns true if the input list is a square
    (i.e., the first half list is equal to the second half list).
  -}
  isPerfectSquare :: String -> Bool
  isPerfectSquare xs = even (List.length xs) && Tuple.fst s == Tuple.snd s
    where
      s = balancedSplit xs

  {-|
    The 'isShuffleSquare' function returns true if the input string is a square
    w.r.t. to the shufle product.
  -}
  isShuffleSquare :: String -> Bool
  isShuffleSquare = not . List.null . shuffleSquareRoots

  {-|
    The 'shuffleSquareRoots' function returns the list of the shuffle square
    roots of an input list.
  -}
  shuffleSquareRoots :: String -> [String]
  shuffleSquareRoots xs
    | odd n              = []
    | oddParikh xs       = []
    -- | isPerfectSquare xs = ["perfect"]
    | otherwise          = shuffleSquareRootsAux a n
    where
      n = List.length xs
      a = Array.array (0, n-1) $ List.zip [0..] xs

  shuffleSquareRootsAux :: Array.Array Int Char -> Int -> [String]
  shuffleSquareRootsAux a n = shuffleSquareRootsStep 2 a n t
    where
      a' = Array.array (0, 0) [(0, a Array.! 0)]
      t  = Array.array (0, 0) [(0, Set.singleton a')]

  shuffleSquareRootsStep :: Int -> Array.Array Int Char -> Int -> Array.Array Int (Set.Set (Array.Array Int Char)) -> [String]
  shuffleSquareRootsStep i a n t
    | i > n     = fmap Array.elems . Set.toList $ t Array.! (n `div` 2)
    | otherwise = shuffleSquareRootsStep (i+1) a n t'
    where
      t' = mkArrayshuffleSquareRootsStep i a n t

  mkArrayshuffleSquareRootsStep :: Int -> Array.Array Int Char -> Int -> Array.Array Int (Set.Set (Array.Array Int Char)) -> Array.Array Int (Set.Set (Array.Array Int Char))
  mkArrayshuffleSquareRootsStep i a n t = Array.array (lb, ub) assocs
    where
      lb     = max 0 (i - (n `div` 2))
      ub     = i `div` 2
      assocs = [(j, mkSet j) | j <- [lb..ub]]

      mkSet j = Set.union set1 set2
        where
          set1 = Set.fromList $ mkSetAux1 j
          set2 = Set.fromList $ mkSetAux2 j

      mkSetAux1 j
        | j > 0 = [a' | a' <- Set.toList $ t Array.! (j-1)
                      , a' Array.! (j-1) == a Array.! (i-1)]
        | otherwise = []

      mkSetAux2 j
        | j <= b = [Array.array (lb', ub'+1) assocs | a' <- Set.toList $ t Array.! j
                                                    , let (lb', ub') = Array.bounds a'
                                                    , let assoc  = (ub'+1, a Array.! (i-1))
                                                    , let assocs = assoc : Array.assocs a']
        | otherwise = []
        where
          b = min ((i-1) `div` 2) (n `div` 2)

  isSquareShuffleFree :: String -> Bool
  isSquareShuffleFree = not . Foldable.any isShuffleSquare . evenLengthPrefixes

  isSquareShuffleFreeF :: Dictionary -> Dictionary -> String -> (Bool, Dictionary, Dictionary)
  isSquareShuffleFreeF allowed forbidden = isSquareShuffleFreeFAux allowed forbidden . evenLengthPrefixes

  isSquareShuffleFreeFAux :: Dictionary -> Dictionary -> [String] -> (Bool, Dictionary, Dictionary)
  isSquareShuffleFreeFAux allowed forbidden [] = (True, allowed, forbidden)
  isSquareShuffleFreeFAux allowed forbidden (p : ps)
    | Set.member p allowed   = isSquareShuffleFreeFAux allowed forbidden ps
    | Set.member p forbidden = (False, allowed, forbidden)
    | isShuffleSquare p      = (False, allowed, Set.insert (List.reverse p) $ Set.insert p forbidden)
    | otherwise              = isSquareShuffleFreeFAux (Set.insert (List.reverse p) $ Set.insert p allowed) forbidden ps

  backward :: Alphabet -> String -> String
  backward alphabet [] = []
  backward alphabet (x : xs) =
    case nextLetter alphabet x of
      Nothing -> backward alphabet xs
      Just x' -> x' : xs

  forward :: String -> String
  forward xs = firstLetter : xs

  explore :: Alphabet -> String -> IO ()
  explore alphabet = aux
    where
      aux [] = print "done."
      aux xs =
        if isSquareShuffleFree xs
        then do
          putStrLn $ show (List.length xs) `Monoid.mappend` ": " `Monoid.mappend` show xs
          aux $ forward xs
        else aux $ backward alphabet xs

  exploreF :: Alphabet -> String -> IO ()
  exploreF alphabet = aux Set.empty Set.empty
    where
      aux _       _         [] = print "done."
      aux allowed forbidden xs =
        let (res, allowed', forbidden') = isSquareShuffleFreeF allowed forbidden xs in
          if res
          then do
            putStrLn $ show (List.length xs)      `Monoid.mappend`
                       ". (allow: "               `Monoid.mappend`
                       show (Set.size allowed')   `Monoid.mappend`
                       ", forbidden: "            `Monoid.mappend`
                       show (Set.size forbidden') `Monoid.mappend`
                       "):\n"                     `Monoid.mappend`
                       show xs
            hFlush stdout
            aux allowed' forbidden' $ forward xs
          else aux allowed' forbidden' $ backward alphabet xs

  main :: IO ()
  main = do
    let axiom = "a"
    exploreF (mkAlphabet 4) axiom
