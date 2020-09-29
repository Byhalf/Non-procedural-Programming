data Prop = 
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

eval :: Prop -> Bool
eval (And x y) = (eval x) && (eval y)
eval (Or x y) = (eval x) || (eval y)
eval (Not x) = not (eval x)
eval (Const x) = x
eval (Var x) = error "variable"

getVar' :: Prop -> [Char]
getVar' (And x y) = (getVar' x) ++ (getVar' y)
getVar' (Or x y) = (getVar' x) ++ (getVar' y)
getVar' (Not x) =  (getVar' x)
getVar' (Var x) = [x]

dupdelete :: Eq a =>[a] -> [a]
dupdelete (l:ls) = if (elem l ls) then dupdelete ls else l: dupdelete ls
dupdelete [] = []
getVar :: Prop -> [Char]
getVar p = dupdelete $ getVar' p

assignOneConst :: Char -> Bool -> Prop -> Prop
assignOneConst c b (And x y) = let f = assignOneConst c b
                               in And (f x) (f y)
assignOneConst c b (Or x y) = let f = assignOneConst c b
                               in Or (f x) (f y)
assignOneConst c b (Not x) = Not (assignOneConst c b x)
assignOneConst c b (Var c2) = if c==c2 then (Const b) else Var c2 
assignOneConst c b (Const c2) = Const c2

assignConst :: Prop -> [Char] -> [Bool]  -> Prop
assignConst p (c:cs) (b:bs)  = assignOneConst c b (assignConst p cs bs)
assignConst p [] []  = p


allBools :: [Char] -> [[Bool]]
allBools [] = [[]]
allBools (x:xs) = [True:r | r <- allBools xs] ++ [False:r | r <- allBools xs]

isTaut :: Prop -> Bool
isTaut p = all (==True) $ map eval $ map (assignConst p (getVar p)) (allBools (getVar p)) 

--isTaut (Or (And (Not (Var 'a')) (Not (Var 'b'))) (Or (Var 'a') (Var 'b')) )