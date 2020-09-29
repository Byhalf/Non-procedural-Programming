import System.Environment   
import Text.Printf
import Data.List
-- An infinite stream of pseudorandom numbers in the range [0 .. 3].
pseudo_random :: [Int]
pseudo_random =
    let f :: Integer -> Integer
        f x = (6364136223846793005 * x + 1442695040888963407) `mod` (2 ^ 64)
        stream = iterate f 1
    in map (\n -> fromIntegral (n `div` (2 ^ 62))) stream


is_on_raft :: Int -> (Int,Int) -> Bool
is_on_raft n (x,y) = x<=n && x>=(-n) && y<=n && y>= (-n)


move_one :: Int -> (Int,Int) -> (Int,Int)
move_one 0 (x,y) = (x,y+1)
move_one 1 (x,y) = (x,y-1)
move_one 2 (x,y) = (x-1,y)
move_one 3 (x,y) = (x+1,y)
move_one _ (x,y) = (x,y) --this is ugly

moves :: Int -> [Int] -> [(Int,Int)] -> (Bool ,[Int],[(Int,Int)])
moves n (l:ls) ((x,y):ps)
    |new_pos == (0,0) = (True, ls, ((x,y):ps))
    |is_on_raft n new_pos = moves n ls (new_pos:(x,y):ps)
    |otherwise = (False, ls, ((x,y):ps))
    where
        new_pos = move_one l (x,y)


walk :: Int -> (Int, [Int],[(Int,Int)]) -> (Int, [Int],[(Int,Int)])
-- raft size -> (success, random nbrs)
walk n (s,rdms,_) = 
    let (b,new_rdms,pos) = moves n rdms [(0,0)]
    in if b then (s+1,new_rdms,pos) else (s, new_rdms, pos)

--- stack overflow code ------
percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
-- roundToStr n f = printf ("%0." ++ show n ++ "f") f
------------------------------------------

-- get_bare_raft :: Int -> [String]
-- get_bare_raft n = replicate [replicate "." (n*2+1)] (n*2+1)

convert_pos :: Int -> (Int,Int)-> (Int,Int)
convert_pos n (x,y) = (n+x,n+y)

--positions have to be sorted
make_raft :: Int -> (Int,Int) -> [(Int,Int)] -> [String] -> [String]
make_raft n (_,-1) _ s = s
make_raft n (x1,y1) ((x2,y2):ps) (s:ls)
    |x1>=0 && y1>=0 = if x1==x2 && y1==y2 then make_raft n (x1-1,y1) ps ((s++"+"):ls) else make_raft n (x1-1,y1) ((x2,y2):ps) ((s++"."):ls) --is there better way than s++"string"
    |x1<0 = make_raft n (n,y1-1) ((x2,y2):ps) ("":s:ls)
--    |otherwise = (s:ls) --should be useless <=> y1< 0 = s 
make_raft n (x1,y1) [] (s:ls) 
    |x1>=0 && y1>=0 = make_raft n (x1-1,y1) [] ((s++"."):ls)
    |x1<0 = make_raft n (n,y1-1) [] ("":s:ls)



-- change_raft :: [(Int,Int)] -> [String] -> String
-- change_raft (p:ps) s= 

pos_sort :: (Int, Int) -> (Int, Int) -> Ordering
pos_sort (a,b) (c,d)
    | b > d = LT
    | b == d && a>c = LT
    | otherwise = GT 
-- stack overflow code --
removeDuplicates ::  Eq a => [a] -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []
----------------------


get_raft :: Int -> [(Int,Int)] -> [String]
get_raft n pos = 
    let raft_pos = sortBy pos_sort ( map (convert_pos n) ( removeDuplicates pos))
        raft_size = (n*2+1) -1
    in reverse $ map reverse $ make_raft raft_size  (raft_size ,raft_size ) raft_pos [""]


main = do
    content <- getContents
    let (sn : t : _) = lines content
        n = read sn
        raft_size = (n*2+1) -1
        applied_walk = walk n
        solutions = iterate applied_walk (0, pseudo_random,[(0,0)])
        (solution,_,pos) = iterate applied_walk (0, pseudo_random,[(0,0)]) !! (read t)
        (_,_,coords) = unzip3 $ take ((read t)+1) solutions 

        (_:strings) = map (get_raft n) coords 
    mapM_ (mapM_ putStrLn) strings
    printf "survived %0.1f%% (%d / %s)\n"  (percent solution $ read t) solution t
--survived 66.7% (2 / 3)