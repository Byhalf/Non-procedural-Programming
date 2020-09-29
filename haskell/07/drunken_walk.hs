import System.Environment   
import Text.Printf

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

moves :: Int -> [Int] -> (Int,Int) -> (Bool ,[Int])
moves n (l:ls) (x,y)
    |new_pos == (0,0) = (True, ls)
    |is_on_raft n new_pos = moves n ls new_pos
    |otherwise = (False, ls)
    where new_pos = move_one l (x,y)


walk :: Int -> (Int, [Int]) -> (Int, [Int])
-- raft size -> (success, random nbrs)
walk n (s,rdms) = 
    let (b,new_rdms) = moves n rdms (0,0)
    in if b then (s+1,new_rdms) else (s, new_rdms)

--- stack overflow code ------
percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
-- roundToStr n f = printf ("%0." ++ show n ++ "f") f
------------------------------------------

main = do
    content <- getContents
    let [ n, t] = lines content
    let applied_walk = walk (read n)
    let (solution,_) = iterate applied_walk (0, pseudo_random) !! (read t)
    printf "survived %0.1f%% (%d / %s)\n"  (percent solution $ read t) solution t
    return ()
--survived 66.7% (2 / 3)
