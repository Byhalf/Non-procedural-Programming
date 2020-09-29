
import System.Environment   
-- sequences :: Int -> [a] -> [[a]]
-- sequences 0 _ = []
-- sequences n l = sequences (n-1) l ++ concatMap (perms n) (combinations n l ) 


-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _ = [[]]  -- there is one subset with 0 elements: the empty set
-- combinations k [] = []   -- there are no subsets of [] with more than 0 elements
-- --combinations k (x : l) = [ x : m | m <- combinations (k - 1) l] ++ (combinations k l)
-- combinations k (x : l) = [x : m | m <- combinations (k - 1) l] ++ combinations k l

-- perms ::  Int ->[a] -> [[a]]
-- perms 0 _ = [[]]
-- perms n l = [ x : m | x <- l, m <- perms (n-1) l ]

-- perms' :: [a] -> [[a]]
-- perms' [] = [[]]
-- perms' (x : l) = [ a ++ (x : b) | m <- perms' l, k <- [0 .. length m], let (a, b) = splitAt k m ]

sequences' :: Int -> [a] -> [[a]]
sequences' 0 _ = [[]]
sequences' n l =  [x : s|x<-l,  s <- sequences' (n-1) l] 

sequences :: Int -> [a] -> [[a]]
sequences n l = concatMap (`sequences'` l) [1..n]


-- useless zip3 and unzip3
-- blockMaker :: [a] -> [a] -> [(Int,a,a)]
-- blockMaker l s  = blockMaker' l s 0 --s and l always same length

-- blockMaker' :: [a] -> [a] ->Int-> [(Int,a,a)]
-- blockMaker' [] [] _ = []
-- blockMaker' (l:ls) (m:ms)  n = (n, l, m) : blockMaker' ls ms (n + 1)

blockZip :: [a] -> [a] -> [(Int,a,a)]
blockZip l s = let n= length l in zip3 [1..n+1] l s

--let a =["a","ab","bba"] , let b =["baa","aa","bb"]
--sequences 3 (blockZip a b)
solution :: [[(Int,String,String)]] -> [Int]
solution [] = []
solution (l:ls) = let (a,b,c) = unzip3 l in if  concat b == concat c then a else solution ls

getsolution :: Int -> [String] -> [String] -> [Int]
getsolution n a b = solution (sequences n (blockZip a b))

-- 5
-- a ab bba
-- baa aa bb
main = do
    content <- getContents
    let [nbr, a, b] = lines content
    let solution = getsolution (read nbr) (words a) (words b)
    if solution == [] 
        then putStr "no solution "
        else putStr $ unwords$ map show $ solution
