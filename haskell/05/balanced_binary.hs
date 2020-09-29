data Tree = Nil | Node Tree Int Tree
  deriving (Eq, Ord, Show)


-- allBalanced 2
--[Node Nil 1 (Node Nil 2 Nil),Node (Node Nil 1 Nil) 2 Nil]

emptyTree :: Int -> Tree
emptyTree x = Node Nil x Nil

allBalanced :: Int -> [Tree]
allBalanced 0 = [emptyTree 0]
allBalanced x = allBalanced' [1..x]

allBalanced' :: [Int] -> [Tree]
allBalanced' [] = []
allBalanced' [x] = [emptyTree x]
allBalanced' [x1,x2] = [insertChild (emptyTree x1) (emptyTree x2), insertChild (emptyTree x2) (emptyTree x1) ]
allBalanced' l
    | odd (length l) = concat ([map (joinTree root left) (allBalanced' [x| x<- l, x > root]) | left <- (allBalanced' [x| x<- l, x < root])])
    | otherwise = concat ([map (joinTree root left) (allBalanced' [x| x<- l, x > root]) | left <- (allBalanced' [x| x<- l, x < root])]
                ++ [map (joinTree (root-1) left) (allBalanced' [x| x<- l, x > (root-1)]) | left <- (allBalanced' [x| x<- l, x < (root-1)])])
--      zipWith (joinTree root) (allBalanced' [x| x<- l, x < root]) (allBalanced' [y| y <-l, y > root])
--                 ++ ( zipWith (joinTree (root+1)) (allBalanced' [x| x<- l, x < (root+1)]) (allBalanced' [y| y <-l, y > (root+1)]) )
    where 
        root = (l !! ((length l) `div` 2))


joinTree :: Int -> Tree -> Tree -> Tree
joinTree x t1 t2 = Node t1 x t2
insertDad :: Tree -> [Tree] -> [Tree]
insertDad t l = map (insertChild t) l

insertChild :: Tree -> Tree -> Tree
insertChild (Node lt x rt) (Node la a ra)
    | a > x = Node lt x (Node la a ra) 
    | a < x = Node (Node la a ra)  x rt
    | otherwise = Node lt x rt


