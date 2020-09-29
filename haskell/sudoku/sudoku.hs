import Data.List

data Sudoku  = Sudoku { horizontal :: [String]
                        ,vertical :: [String]
                        ,squares :: [String]} deriving (Eq)

sudokuMaker :: [String] -> Sudoku 
sudokuMaker l = Sudoku {horizontal=l,vertical=transpose l,squares=getSquares l}

makeMove :: Sudoku -> ((Int,Int),String) -> Sudoku
makeMove (Sudoku h v sq) ((x,y), symb)=
    let newLine = take x (h!!y) ++ symb ++ drop (x+1) (h!!y)
        in sudokuMaker $take y h ++ [newLine]  ++ drop (y+1) h


--useless?------------
-- makeObviousMoves :: Sudoku -> [((Int,Int),String)] -> Sudoku
-- makeObviousMoves s [] = s
-- makeObviousMoves s (((x,y),symb):ls) = 
--     if length symb == 1 
--     then makeObviousMoves (makeMove s ((x,y),symb) ) ls
--     else makeObviousMoves s ls


-- makeObviousMoves' :: Sudoku -> [((Int,Int),String)] -> Sudoku
-- makeObviousMoves' s l = let l' = filter (\x-> length (snd x )== 1) l
--                         in makeMoves s l'

----------------------

makeMoves :: Sudoku -> [((Int,Int),String)] -> Sudoku
makeMoves = foldl makeMove

keepMakingObviousMoves :: Sudoku -> Sudoku
keepMakingObviousMoves s = 
    let obviousMoves =  filter (\x-> length (snd x )== 1) $ possibilityList s
        in 
            if not (null obviousMoves)
            then keepMakingObviousMoves $ makeMoves s obviousMoves
            else s



-- isFinished :: Sudoku -> Bool
-- isFinished s = not $ any (any (=='0')) (horizontal s)

getBestMove ::  [((Int,Int),String)] ->((Int,Int),String)
getBestMove  = minimumBy (\x y -> if length (snd x)> length (snd y) then GT else LT)  

solveSudoku :: Sudoku -> Sudoku
solveSudoku s =  
    let 
        allMoves = possibilityList s
        bestMove =  getBestMove allMoves
        remainingMoves = allMoves \\ [bestMove]
        
    in
        solveSudoku' s bestMove allMoves
 

solveSudoku' :: Sudoku -> ((Int,Int),String) -> [((Int,Int),String)] -> Sudoku
solveSudoku' s _ [] = s
solveSudoku' s ((x,y), m) ls 
  | length m == 1 =
      let 
        updatedList = updatePossibilityList ((x, y), m) ls
        f2sudoku = solveSudoku' (makeMove s ((x, y), m)) (getBestMove updatedList) updatedList in
          if isItValid f2sudoku then f2sudoku else sudokuMaker $ replicate 9 $ replicate 9 '1'
  | isItValid s = s

  | isValidMove s ((x, y), [head m]) =
    let 
      movedS = makeMove s ((x, y), [head m])
      updatedList = updatePossibilityList ((x, y), m) ls in
        if isItValid movedS then movedS else
          let fsudoku = solveSudoku' movedS  (getBestMove updatedList) updatedList in
            if isItValid fsudoku then fsudoku else
              solveSudoku' s ((x, y), tail m) ls 

  | otherwise = solveSudoku' s ((x, y), tail m) ls 

splitMoves :: [((Int,Int),String)] -> [((Int,Int),String)]
splitMoves [] = []
splitMoves l = concatMap splitMove l

updatePossibilityList :: ((Int,Int),String) ->  [((Int,Int),String)] -> [((Int,Int),String)]
updatePossibilityList m [] = []
updatePossibilityList ((x1,y1),m) (((x2,y2),m2):ls) 
  | x1==x2 && y1==y2 = updatePossibilityList ((x1,y1),m) ls
  | x1==x2 || y1==y2 || (3*div y1 3 + div x1 3) == (3*div y2 3 + div x2 3)= 
    if m2\\m /= "" then ((x2,y2), m2\\m ):updatePossibilityList ((x1,y1),m) ls else []
  |otherwise = ((x2,y2),m2):updatePossibilityList ((x1,y1),m) ls





isValidMove :: Sudoku -> ((Int,Int),String) -> Bool
isValidMove s m = isItMaybeValid $ makeMove s m  --very ineficient 

--Is the sudoku valid and finished
isItValid :: Sudoku -> Bool
isItValid (Sudoku h v sq) = and $ map validHelper h ++ map validHelper v ++ map validHelper sq

validHelper :: String -> Bool
validHelper s = "123456789" `intersect` s == "123456789"

splitMove :: ((Int,Int),String) -> [((Int,Int),String)]
splitMove ((_,_),"") = []
splitMove ((x,y),s:ls) = ((x,y), [s]): splitMove ((x,y),ls)


--does it respect sudoku rules even if it cant be finished
isItMaybeValid :: Sudoku -> Bool
isItMaybeValid (Sudoku h v sq) = and $ map validHelper' h ++ map validHelper' v ++ map validHelper' sq


validHelper' :: String -> Bool
validHelper' s = let s' = filter (/='0') s in nub s' == s'

----------------------------------------------------------------------------------------------------------


-- instance (Show q) => Show (Sudoku q) where --This doesnt work...
--     show (Sudoku s _ _) =  toStringHori s 

possibilityList :: Sudoku -> [((Int,Int),String)]
possibilityList s = filter (\x-> snd x /= "") [possibilitiesSquare x y s | x <- [0 .. 8], y <- [0 .. 8]]


-- possibilityList' :: Int-> Int -> Sudoku -> [((Int,Int),String)]
-- possibilityList' 8 8 s = [possibilitiesSquare 8 8 s]
-- possibilityList' 8 y s = possibilitiesSquare 8 y s : possibilityList' 0 (y+1) s
-- possibilityList' x y s = possibilitiesSquare x y s : possibilityList' (x+1) y s

-- possibilityList' :: Sudoku -> [((Int,Int),String)]
-- possibilityList' s = [possibilitiesSquare x y s | x <- [0 .. 8], y <- [0 .. 8]]

possibileNbrs :: String -> String
possibileNbrs s = "123456789" \\ s

possibilitiesSquare :: Int -> Int -> Sudoku -> ((Int,Int),String)
possibilitiesSquare x y (Sudoku h v sq) =
    if h!!y!!x == '0'
        then
            let posib = intersect ((possibileNbrs $h !! y) `intersect` (possibileNbrs $v !! x))  (possibileNbrs $sq!!(3*div y 3 + div x 3))
            in
                ((x,y),posib)
        else
            ((x,y),"")


toString1Line :: String -> String
toString1Line  =  drop 3 . foldl (\x y-> x++" | "++y) "" . split3 

toString3Squares :: [String] -> String
toString3Squares  = unlines .  map toString1Line 


-- mapM putStrLn $ lines $toStringHori
toStringHori :: [String] -> String
toStringHori = drop 17 . foldl (\x y-> x++"----------------\n"++y) "" . map toString3Squares . split3

split3 :: [a] -> [[a]]
split3 [] = []
split3 s  = take 3 s : split3 (drop 3 s) 

getSquares' :: [[a]] -> [[a]]
getSquares' l = [map (!!0) l , map (!!1) l, map (!!2) l]

getSquares :: [[a]] -> [[a]]
getSquares hori = map concat $ concat $map getSquares' (split3 (map split3 hori))

main = do 
    content <- getContents
    let hori = lines content
    -- let t = toStringHori hori
    -- let squares = getSquares hori
    -- let verti = transpose hori
    -- mapM putStrLn $ lines t
    let sudoku = sudokuMaker hori
    let sudoku2 = solveSudoku sudoku 
    --print $  sortBy (\x y -> if length (snd x)> length (snd y) then GT else LT) $ possibilityList sudoku
    putStrLn ""
    mapM_ putStrLn $ lines $ toStringHori  hori
    putStrLn ""
    print $ isItMaybeValid sudoku2
    print $ isItValid sudoku2
    putStrLn ""
    mapM putStrLn $ lines $ toStringHori  (horizontal sudoku2)

--more like is it potentially solvable
isItSolvable :: Sudoku -> Bool
isItSolvable s = 
    let 
        l = length $ possibilityList s
    in 
        l>0 && l == length  [x| h<- horizontal s, x<-h,x=='0']