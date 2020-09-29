-- combinations 3 "abcde"
--["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]


combinations :: Int -> [a] -> [[a]]

combinations 0 s = [[]]
combinations _ [] = [[]]
--combinations 1 s = [[x]|x<-s]
--combinations n (x:s) = concat [[x:m ]| m <- combinations (n-1) s]
--combinations n (x:s) = [combinations (n-1) (x:s)]  -- ++ combinations n s 

--combinations n s = concat [[x:m]|x<-s,m <-combinations (n-1) s]

--combinations n (x:s) = [x:[k]| m<-combinations n s , k<-m] 
combinations 2 (x:s) = [x:[k]| k<-s] ++ combinations 2 s 