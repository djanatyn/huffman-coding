-- |
-- Haskell implementation of huffman coding lossless compression.
--
-- - <https://engineering.purdue.edu/ece264/17au/hw/HW13?alt=huffman>
module Huffman (Tree (..)) where

import Data.List (group, sort, sortBy)

-- | Huffman coding tree.
data Tree a where
  Leaf :: a -> Tree a
  Branch :: (Tree a) -> (Tree a) -> Tree a
  deriving (Show)

-- | Frequency of letter occurence in a string.
data Frequency a where
  Frequency :: {char :: a, count :: Int} -> Frequency a
  deriving (Show, Eq)

-- | Sort frequencies by their count.
instance Eq a => Ord (Frequency a) where
  f1 `compare` f2 = count f1 `compare` count f2

-- | Sum frequencies of characters within a tree.
sumCount :: Tree (Frequency a) -> Int
sumCount (Leaf a) = count a
sumCount (Branch a b) = sumCount a + sumCount b

-- | Combine trees by comparing their summed character frequencies.
instance Semigroup (Tree (Frequency a)) where
  a <> b
    | sumCount a == sumCount b = Branch a b
    | sumCount a > sumCount b = Branch b a
    | sumCount a < sumCount b = Branch a b
    | otherwise = error "failed"

-- | Calculate frequency of letters.
frequencies :: String -> [Frequency Char]
frequencies = map (\c -> Frequency {char = head c, count = length c}) . group . sort

-- | Create a Huffman coding tree from a string.
buildTree :: String -> Tree (Frequency Char)
buildTree input =
  let initialTrees :: [Tree (Frequency Char)]
      initialTrees = (fmap Leaf . sort $ frequencies input)

      loop :: [Tree (Frequency Char)] -> Tree (Frequency Char)
      loop [] = error "failed"
      loop [final] = final
      loop next =
        let sorted = sortBy (\a b -> compare (sumCount a) (sumCount b)) next
            pair = take 2 sorted
         in case pair of
              [t1, t2] -> loop $ (Branch t1 t2) : drop 2 sorted
              _ -> error "failed"
   in loop initialTrees

main :: IO ()
main = print $ buildTree "go go gophers"

coding :: Tree (Frequency Char) -> [(Char, String)]
coding input =
  let loop :: Maybe String -> Tree (Frequency Char) -> [(Char, String)]
      loop Nothing start = case start of
        (Leaf end) -> error "only one character"
        (Branch left right) -> loop (Just "0") left ++ loop (Just "1") right
      loop (Just path) next = case next of
        (Leaf end) -> [(char end, path)]
        (Branch left right) -> loop (Just $ path ++ "0") left ++ loop (Just $ path ++ "1") right
   in loop Nothing input
