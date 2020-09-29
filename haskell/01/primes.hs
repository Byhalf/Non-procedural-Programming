factors :: Int -> [Int]
factors(x) =
    if x `mod` 2 == 0
        then [2] ++ factors(x `div` 2)
    else 
        factors'(x,3)

factors'(x,y) = 
    if  y  > ceiling (sqrt(fromIntegral(x)))
        then
            if x>2
                then [x]
            else []
    else 
        if x `mod` y == 0
            then [y] ++ factors'(x `div` y,y)
        else
            factors'(x,y+2)

is_prime :: Int -> Bool
is_prime(n) = (head (factors n) == n)

list_primes :: Int -> [Int]
list_primes(n) = [p|p <- [2..n], is_prime(p)]

pair_maker :: [Int] -> [(Int,Int)]
pair_maker([e1,e2]) = [(e1,e2)]
pair_maker(l:ls) = ( l,head ls):pair_maker(ls)

gap :: Int -> (Int, Int)
gap(n) = gap'(pair_maker(list_primes(n)),0,(0,0))

gap'([],_,solution) = solution
gap'(l:ls,m,solution) =
    if m > (snd l - fst l)
        then gap'(ls,m,solution)
    else
        gap'(ls,snd l - fst l,l)

