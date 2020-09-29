pow :: Int -> (a -> a) -> (a -> a)
pow x f= foldl (.) f $ replicate (x-1) f

add :: Int -> Int -> Int
add a b
    | a/= 0 = (pow a succ) b 
    | otherwise = b
-- because pow 0 succ still adds 1

mul :: Int -> Int -> Int
mul a b
    | a/= 0 = (pow  a $ add b ) 0
    | otherwise = 0

iexp :: Int -> Int -> Int
iexp a b 
    | b == 0 = 1
    | otherwise = (pow b $ mul a) 1