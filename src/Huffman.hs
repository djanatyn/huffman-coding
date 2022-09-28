-- |
-- Haskell implementation of huffman coding lossless compression.
--
-- - <https://engineering.purdue.edu/ece264/17au/hw/HW13?alt=huffman>
module Huffman (Tree (..)) where

import Data.List (group, sort, sortBy)
import Debug.Trace

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
      loop [last] = last
      loop next =
        let
          -- Choose two trees with the smallest weights;
          sorted = sortBy (\a b -> compare (sumCount a) (sumCount b)) next
          -- call these trees T1 and T2.
          [t1, t2] = take 2 sorted
        -- Create a new tree whose...left sub-tree is T1 and whose right
        -- sub-tree is T2.
        in loop $ (Branch t1 t2) : drop 2 sorted
   in loop initialTrees

main :: IO ()
main = print $ buildTree "go go gophers"
