--main = putStrLn "hello, world"  

-- main = do  
--     foo <- putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     putStrLn ("Hey " ++ name ++ ", you rock!")  

-- import Control.Monad  
-- import Data.Char  
  
-- main = forever $ do  
--     putStr "Give me some input: "  
--     l <- getLine  
--     putStrLn $ map toUpper l  

import Control.Monad  
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        getLine )  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors