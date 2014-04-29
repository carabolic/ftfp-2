module Search where

import qualified Data.Set as S
import qualified Data.Trie as T

searchA :: Ord a => ((String, a) -> Bool)                -- Pointwise predicate
                 -> ((String, a) -> (String, a) -> Bool) -- Pairwise predicate
                 -> S.Set String                         -- Domain 
                 -> S.Set a                              -- Range
                 -> ((String, a) -> Bool)                -- Accumulated predicates
                 -> S.Set (T.Trie a)                     -- Solution
searchA p m dom ran acc 
  | S.null dom = S.singleton T.empty
  | otherwise  = S.foldr S.union S.empty (S.map f ran')
  where
    (d, dom') = S.deleteFindMin dom
    ran' = S.filter (\v -> p (d,v)) ran
    f b = if acc (d, b)
            then j (searchA p m dom' ran acc') (T.singleton d b)
            else S.empty
      where
        j sol trie = S.map (`T.insertTrie` trie) sol 
        acc' = m (d, b)

globalSearchA :: Ord a => (a -> Bool)                          -- Range constraint?
                       -> ((String, a) -> Bool)                -- Pointwise predicate
                       -> ((String, a) -> (String, a) -> Bool) -- Pairwise predicate
                       -> S.Set String                         -- Domain
                       -> S.Set a                              -- Range
                       -> S.Set (T.Trie a)                     -- Solution
globalSearchA pr p m var val = searchA p m var (S.filter pr val) (const True)

globalSearchB :: Ord a => ((String, a) -> Bool)                -- Pointwise predicate
                       -> ((String, a) -> (String, a) -> Bool) -- Pairwise predicate
                       -> S.Set String                         -- Domain 
                       -> S.Set a                              -- Range
                       -> S.Set (T.Trie a)                     -- Solution
globalSearchB p m dom ran
  | S.null dom = S.singleton T.empty
  | otherwise  = join m (globalSearchB p m dom' ran) s'
  where
    (d, dom') = S.deleteFindMin dom :: (String, S.Set String)
    -- apply pointwise predicate to the range
    ran' = S.filter (\v -> p (d,v)) ran
    s' = S.map (T.singleton d) ran'

join :: Ord a => ((String, a) -> (String, a) -> Bool) -- predicate
              -> S.Set (T.Trie a)                     -- R
              -> S.Set (T.Trie a)                     -- S
              -> S.Set (T.Trie a)                     -- filter predicate (R x S)
join m r s
  | S.null r  = s
  | S.null s  = r
  | otherwise =
      -- create result set
        S.fromList
      -- apply predicate
      $ filter (T.mutually m)
      -- cartesian product
      $ map (uncurry T.insertTrie) [(a,b) | a <- S.toList r, b <- S.toList s]

solveA :: Int -> S.Set (T.Trie Int)
solveA n
  | n <= 2    = S.singleton T.empty
  | otherwise = globalSearchA rp cp cm dom ran
  where
    -- constraints
    rp = const True
    cp ("a", val) = val /= 2
    cp _          = True 
    cm ("a", val1) ("b", val2) = val1 < val2
    cm ("b", val1) ("a", val2) = val1 > val2
    -- domain and range
    dom = S.fromList ["a", "b"]
    ran = S.fromList [1..n]

solveB :: Int -> S.Set (T.Trie Int)
solveB n 
  | n <= 2    = S.singleton T.empty
  | otherwise = globalSearchB cp cm (S.fromList ["a", "b"]) (S.fromList [1..n])
  where 
    cp ("a", value) = value /= 2
    cp _            = True
    cm ("a", value1) ("b", value2) = value1 < value2
    cm ("b", value1) ("a", value2) = value1 > value2
