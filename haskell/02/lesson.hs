subsets [] = [[]]
subsets (x : l) = concat [ [m, x : m] | m <- subsets l ]

compositions :: Int -> [[Int]]

compositions 0 = [[]]

compositions n = [ k : m | k <- [1 .. n], m <- compositions (n - k) ]

split s = [[x]|x<-s]
test (x:s) = [x:[m]|m<-s]