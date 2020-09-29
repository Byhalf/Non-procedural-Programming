import Data.List

--(jack, queen, king, ace) = (11, 12, 13, 14)

--lengthPairMaker assumes all elements are the same
lengthPairMaker :: [a] -> (Int, a)
lengthPairMaker x = ((length x), (head x))

lesserLengthPairMaker :: (Int, Int) -> (Int, Int) -> Ordering
lesserLengthPairMaker (a,b) (c,d)
    | a == c && b == d = LT
    | a == c && b > d = LT 
    | a /= c && a > c = LT 
    | otherwise = GT


--sortBy lesserLengthPairMaker (map lengthPairMaker (reverse (group (sort [2,3,4,2,3]) )))

transformHand :: [Int] -> [(Int,Int)]
transformHand x = sortBy lesserLengthPairMaker (map lengthPairMaker (reverse (group (sort x) )))

better :: [Int] -> [Int] -> Bool
better x y 
    | (length (transformHand x)) == (length (transformHand y)) = transformHand x > transformHand y
    | otherwise = length (transformHand x) < length (transformHand y)
-- better' :: [(Int,Int)] -> [(Int,Int)] -> Bool
-- better' 
