import Data.List   -- for 'delete' function

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]  -- there is one subset with 0 elements: the empty set
combinations k [] = []   -- there are no subsets of [] with more than 0 elements
--combinations k (x : l) = [ x : m | m <- combinations (k - 1) l] ++ (combinations k l)
combinations k (x : l) = [x : m | m <- combinations (k - 1) l] ++ combinations k l
-- Permutations, first solution using 'delete'
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms l = [ x : m | x <- l, m <- perms (delete x l)]

-- Second possible solution
perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' (x : l) = [ a ++ (x : b) | m <- perms' l, k <- [0 .. length m], let (a, b) = splitAt k m ]

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x : l) = concat [ [ (x : a, b), (a, x : b) ] | (a, b) <- partitions l ]
