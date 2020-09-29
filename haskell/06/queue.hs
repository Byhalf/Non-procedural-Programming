--data Queue a = Queue { front :: [a], back :: [a] } deriving (Show)


class Queue q where 
    emptyQueue :: q a
    isEmpty ::  q a -> Bool
    enqueue :: a -> q a -> q a
    dequeue :: q a -> (a, q a)
-- emptyQueue - an empty queue
-- isEmpty - tests whether a queue is empty, returning a Bool
-- enqueue - enqueue a value, returning a new queue
-- dequeue - dequeue a value, returning the value plus a new queue


data TestQueue q = TestQueue [q] deriving (Show)

 
instance Queue TestQueue where
    emptyQueue = TestQueue []
    isEmpty (TestQueue []) = True
    isEmpty (TestQueue _) = False
    enqueue x (TestQueue xs) = TestQueue (xs ++ [x])
    dequeue (TestQueue []) = error "queue is empty"
    dequeue (TestQueue (x : xs)) = (x, TestQueue xs)

data SQueue a = SQueue [a] [a]

instance Queue SQueue where
    emptyQueue = SQueue [] []
    isEmpty (SQueue [] []) = True  -- why didnt emptyQueue instead of (SQueue [] []) work
    isEmpty _ = False
    enqueue x (SQueue l r) = SQueue l (x:r)
    dequeue (SQueue [] []) = error "queue is empty"
    dequeue (SQueue [] r) = let (l:ls) = reverse r in (l,SQueue ls [])
    dequeue (SQueue (l:ls) r) = (l ,SQueue ls r)


instance (Eq q) => Eq (SQueue q) where
    (SQueue la ra) == (SQueue lb rb) = (la ++ ra) == (lb ++ rb)

instance (Show q) => Show (SQueue q) where
    show (SQueue l r) = "q"++ show (l ++ (reverse r))

--it's a bit weird to me we could modify the Squeue even if it stays technically equal
instance Functor SQueue where
    fmap f (SQueue l r) = SQueue (fmap f l) (fmap f r)

queue_of_nums :: Queue q => Int -> Int -> q Int
queue_of_nums a b = foldl (flip enqueue) emptyQueue  [a..b]


--fmap (+3) (enqueue 2 (enqueue 4 emptyQueue)) :: SQueue Int

-- let q = emptyQueue :: SQueue Int -- how to run this in ghci?
--     q1 = enqueue 5 q
--     q2 = enqueue 10 q1
--     e = isEmpty q2
--     (x, q3) = dequeue q2
--     (y, q4) = dequeue q3
--     f = isEmpty q4
-- in (e, x, y, f)

--q1 = SQueue [6,2] [7,8,11]
