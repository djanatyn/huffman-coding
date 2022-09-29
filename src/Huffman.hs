-- |
-- Haskell implementation of huffman coding lossless compression.
--
-- - <https://engineering.purdue.edu/ece264/17au/hw/HW13?alt=huffman>
module Huffman
  ( -- Types
    Tree (..),
    Frequency (..),
    Dictionary (..),

    -- Creating Trees
    frequencies,
    buildTree,

    -- Interpreting Trees
    coding,
    translate,

    -- Serializing
    splitBytes
  )
where

import Data.List (group, sort, sortBy, unfoldr)
import Data.Maybe (fromMaybe)

-- | Huffman coding tree.
data Tree a where
  Leaf :: a -> Tree a
  Branch :: Tree a -> Tree a -> Tree a
  deriving (Show)

-- | Frequency of letter occurence in a string.
data Frequency a where
  Frequency :: {char :: a, count :: Int} -> Frequency a
  deriving (Show, Eq)

data Dictionary a where
  Dictionary :: [(a, String)] -> Dictionary a
  deriving (Show)

instance Semigroup (Dictionary a) where
  Dictionary a <> Dictionary b = Dictionary (a <> b)

-- | Sort frequencies by their count.
instance Eq a => Ord (Frequency a) where
  f1 `compare` f2 = count f1 `compare` count f2

-- | Sum frequencies of characters within a tree.
sumCount :: Tree (Frequency a) -> Int
sumCount (Leaf a) = count a
sumCount (Branch a b) = sumCount a + sumCount b

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
              [t1, t2] -> loop $ Branch t1 t2 : drop 2 sorted
              _ -> error "failed"
   in loop initialTrees

coding :: Tree (Frequency Char) -> Dictionary Char
coding input =
  let loop :: Maybe String -> Tree (Frequency Char) -> Dictionary Char
      loop Nothing start = case start of
        (Leaf _) -> error "only one character"
        (Branch left right) ->
          loop (Just "0") left <> loop (Just "1") right
      loop (Just path) next = case next of
        (Leaf end) -> Dictionary [(char end, path)]
        (Branch left right) ->
          loop (Just $ path ++ "0") left <> loop (Just $ path ++ "1") right
   in loop Nothing input

grabByte :: String -> Maybe (String, String)
grabByte bytes =
  let byte = take 8 bytes
      rest = drop 8 bytes
   in if null byte then Nothing else Just (byte, rest)

-- | Translate to binary string using Huffman coding.
translate :: Dictionary Char -> String -> String
translate (Dictionary dict) input =
  let get :: Char -> String
      get c = fromMaybe (error "failed") $ lookup c dict
   in foldl1 (++) $ map get input

splitBytes :: String -> [String]
splitBytes = unfoldr grabByte

main :: IO ()
main =
  let input = "recurse center"
      tree = buildTree input
      dict = coding tree
      translation = translate dict input
      bytes = splitBytes translation
   in mapM_ putStrLn [input, show tree, show dict, show translation, show bytes]
