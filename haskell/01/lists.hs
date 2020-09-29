-- flattens a list of lists, i.e. combines all subelements into a single list:
flatten :: [[a]] -> [a]
flatten([]) = []
flatten (l:ls) = l ++ flatten(ls)

--  dezips a list of pairs into a pair of lists
dezip :: [(a, b)] -> ([a], [b])
dezip([]) = ([],[])
dezip(c:cs) =
    let l = dezip(cs)
        l1 = [fst c]  ++ fst(l) 
        l2 = [snd c]  ++ snd(l)
    in (l1,l2)

-- reverses a list O(n):
rev :: [a] -> [a]
rev(l) = revH(l,[])

--naive?
rev'([]) = []
rev'(l:ls) = rev(ls) ++ [l]

--accumulator
--rev(l) = rev(l,a)


revH([],a) = a
revH(l:ls,a) =  revH(ls,l:a)