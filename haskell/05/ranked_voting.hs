import Data.List


elect :: [[String]] -> [String]
elect l
    | and $ map (\x -> x == (head l)) l = [least] 
    | otherwise = least :( elect $ deleteEmpty $ map (filter (/=least)) l)
    where least = leastVotes $ foldl union [] l ++[ x!!0 | x <- l ]

deleteEmpty :: [[a]] -> [[a]]
deleteEmpty (l:ls)
    | length l == 0 = deleteEmpty ls
    | otherwise = l : deleteEmpty ls 
deleteEmpty [] = []

leastVotes :: [String] -> String
leastVotes l =  head $ head $ sortBy (\x y ->if length x> length y then GT else LT ) $ group $ sort l 


-- elect [["a"],["b"],["a"]]

-- elect [[ "red", "green" ],[ "blue" ],[ "green", "yellow", "red", "blue" ],[ "blue", "green", "yellow", "red" ],[ "green" ]]