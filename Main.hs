module Main

where

  import System.IO
  import qualified Data.Tuple      as T
  import qualified Data.List       as L
  import qualified Data.Set        as S
  import qualified Data.Foldable   as F
  import qualified Data.Array      as Array
  import qualified Data.Monoid     as Monoid
  import qualified Data.Char       as Char
  import qualified Data.Map.Strict as Map
  import qualified Control.Arrow   as Arrow

  newtype Alphabet = Alphabet { getSize :: Int }

  type Dictionary = S.Set String

  type CharArray = Array.Array Int Char
  type DPSet     = S.Set CharArray
  type DPArray   = Array.Array Int DPSet

  type Message = (Bool, Dictionary, Dictionary)

  -- Make an alphabet of given size.
  mkAlphabet :: Int -> Alphabet
  mkAlphabet n
    | n <= 0    = Alphabet { getSize = 0 }
    | otherwise = Alphabet { getSize = n }

  -- Return the first letter of the alphabet.
  firstChar :: Char
  firstChar = 'a'

  -- Return the last letter of the alphabet.
  lastChar :: Alphabet -> Char
  lastChar Alphabet { getSize = s } = Char.chr $ Char.ord 'a' + (s-1)

  -- Return the next letter.
  nextLetter :: Alphabet -> Char -> Maybe Char
  nextLetter alphabet c
    | c < firstChar          = Nothing
    | c >= lastChar alphabet = Nothing
    | otherwise              = Just $ Char.chr ((1 + Char.ord c - Char.ord 'a') + Char.ord 'a')

  -- Return the list of all even length prefixes of 'xs'.
  evenPrefixes :: String -> [String]
  evenPrefixes = fmap T.snd . L.filter (even . T.fst) . L.zip [1..] . L.tail . L.inits

  -- Returns true iff each element has an even number of occurrences.
  evenParikh :: String -> Bool
  evenParikh = F.all (even . T.fst) . fmap (F.length Arrow.&&& L.head) . L.group . L.sort

  -- Return true if the input string is a perfect square.
  perfectSquare :: String -> Bool
  perfectSquare xs = T.uncurry (==) $ L.splitAt (L.length xs `div` 2) xs

  -- Return true if the input string is a square w.r.t. to the shufle product.
  shuffleSquare :: String -> Bool
  shuffleSquare xs = perfectSquare xs || (not . L.null $ shuffleSquareRoots xs)

  -- Rreturn all shuffle square roots.
  shuffleSquareRoots :: String -> [String]
  shuffleSquareRoots xs
    | odd n               = []
    | not (evenParikh xs) = []
    | otherwise           = shuffleSquareRoots' a n
    where
      n = L.length xs
      a = Array.array (0, n-1) $ L.zip [0..] xs

  shuffleSquareRoots' :: Array.Array Int Char -> Int -> [String]
  shuffleSquareRoots' a n = shuffleSquareRootsStep 2 a n t
    where
      a' = Array.array (0, 0) [(0, a Array.! 0)]
      t  = Array.array (0, 0) [(0, S.singleton a')]

  shuffleSquareRootsStep :: Int -> Array.Array Int Char -> Int -> DPArray -> [String]
  shuffleSquareRootsStep i a n t
    | i > n     = fmap Array.elems . S.toList $ t Array.! (n `div` 2)
    | otherwise = shuffleSquareRootsStep (i+1) a n t'
    where
      t' = mkArrayshuffleSquareRootsStep i a n t

  mkArrayshuffleSquareRootsStep :: Int -> Array.Array Int Char -> Int -> DPArray -> DPArray
  mkArrayshuffleSquareRootsStep i a n t = Array.array (lb, ub) assocs
    where
      lb     = max 0 (i - (n `div` 2))
      ub     = i `div` 2
      assocs = [(j, mkSet j) | j <- [lb..ub]]

      mkSet j = S.union set1 set2
        where
          set1 = S.fromList $ mkSetAux1 j
          set2 = S.fromList $ mkSetAux2 j

      mkSetAux1 j
        | j > 0 = [a' | a' <- S.toList $ t Array.! (j-1)
                      , a' Array.! (j-1) == a Array.! (i-1)]
        | otherwise = []

      mkSetAux2 j
        | j <= b = [Array.array (lb', ub'+1) assocs | a' <- S.toList $ t Array.! j
                                                    , let (lb', ub') = Array.bounds a'
                                                    , let assoc  = (ub'+1, a Array.! (i-1))
                                                    , let assocs = assoc : Array.assocs a']
        | otherwise = []
        where
          b = min ((i-1) `div` 2) (n `div` 2)

  -- Backtrack for next string.
  backward :: Alphabet -> String -> String
  backward alphabet []       = []
  backward alphabet (x : xs) = case nextLetter alphabet x of
                                 Nothing -> backward alphabet xs
                                 Just x' -> x' : xs

  -- Forward for next string.
  forward :: String -> String
  forward xs = firstChar : xs

  explore :: Alphabet -> String -> IO ()
  explore alphabet = aux S.empty S.empty
    where
      aux _       _         []  = error "unexpected empty buffer"
      aux allowed forbidden xs  =
        if L.length xs == 1 && xs /= [firstChar]
        then print "done."
        else
          let (res, allowed', forbidden') = explore' allowed forbidden xs in
            if res
            then do
              putStrLn $ show (L.length xs)       `Monoid.mappend`
                         ". (allow: "             `Monoid.mappend`
                         show (S.size allowed')   `Monoid.mappend`
                         ", forbidden: "          `Monoid.mappend`
                         show (S.size forbidden') `Monoid.mappend`
                         "):\n"                   `Monoid.mappend`
                         show xs
              hFlush stdout
              aux allowed' forbidden' $ forward xs
            else aux allowed' forbidden' $ backward alphabet xs

  explore' :: Dictionary -> Dictionary -> String -> Message
  explore' allowed forbidden = aux allowed forbidden . evenPrefixes
    where
      aux allowed' forbidden' [] = (True, allowed', forbidden')
      aux allowed' forbidden' (p : ps)
        | S.member p allowed'   = aux allowed' forbidden' ps
        | S.member p forbidden' = (False, allowed', forbidden')
        | shuffleSquare p       = (False, allowed', S.insert (L.reverse p) $ S.insert p forbidden')
        | otherwise             = aux (S.insert (L.reverse p) $ S.insert p allowed') forbidden' ps

  main :: IO ()
  main = let axiom = "a" in explore (mkAlphabet 4) axiom
