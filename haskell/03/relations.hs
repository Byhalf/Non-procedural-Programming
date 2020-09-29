-- 
import Data.List
is_reflexive  :: (a -> a -> Bool) -> [a] -> Bool
is_reflexive f l = is_reflexive' f l l

is_reflexive' :: (a -> a -> Bool) -> [a] -> [a] -> Bool
is_reflexive' _ [] _ = True
is_reflexive' f (x:xs) l = (f x x )&& (is_reflexive' f xs l)


get_related :: (a -> a -> Bool) -> [a] -> a -> [a]
get_related f l x = [s| s <- l , f x s] 

--(map (get_related (\x y -> x `mod` 5 == y `mod` 5) [1..30]) [1..30])
--(\x y -> x `mod` 5 == y `mod` 5)
-- is_symmetric :: (a -> a -> Bool) -> [a] -> Bool

unpack :: [[a]] -> [a]
unpack (l:ls) = l ++ unpack ls
unpack [] = [] 


remove_dup :: Eq a => [[a]] -> [[a]]
remove_dup [] = []
remove_dup (l:ls)
    | elem l ls = remove_dup ls
    | otherwise = l : remove_dup ls

classes :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
classes f l = remove_dup (map (get_related f l) l)

is_equiv :: Eq a => (a -> a -> Bool) -> [a] -> Bool
is_equiv f l 
    | length (unpack (classes f l)) == length l = (intersect l(unpack (classes f l)) ) == l
    | otherwise = False

-- is_transitive :: (a -> a -> Bool) -> [a] -> Bool


