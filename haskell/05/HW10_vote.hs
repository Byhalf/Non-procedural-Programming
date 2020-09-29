import Data.List

remove_empty :: Eq a => [[a]] -> [[a]] 
remove_empty = filter (/= [])

eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/=x))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]
   
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

elect2 :: Ord a => [[a]] -> [a]
elect2 bs = elect bs ++ elect2 (eliminate (elect bs) bs)
elect2 [] = []


elect :: Ord a => [[a]] -> [a]
elect bs = case rank (remove_empty bs) of
    [c] -> [c]
    --(c:cs) -> elect (eliminate c bs) 
    (c:cs) -> elect bs -- if you want a list with all then in the recursion you 
                        --need a c: (elect bs) with c being the value you want to save

--so if elect founds the first but not in a list why don't you just delete the first
--and recall elect on what is left
