import Data.Char
import Data.List


mydiv :: Int -> Int -> Double
mydiv x y = (fromIntegral x) / (fromIntegral y)

en_freq :: [Double]
en_freq = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
           6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7,
           7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8,
           1.0, 2.4, 0.2, 2.0, 0.1]
freq_caracter :: Int -> String -> Char -> Double
freq_caracter n m c = mydiv (length $ filter (==c) m) n 

freq_table_maker :: String -> [Double]
freq_table_maker m = [ freq_caracter (length m) m c |c <-['a'..'z']]

chi_calculator :: [Double] -> [Double] -> Double
chi_calculator fr1 fr2 = sum $ map (\(x,y) -> (x-y)^2/y) $ zip fr1 fr2

encrypt :: Int -> String -> String
encrypt n m = map (encryptLetter n) m  

encryptLetter :: Int -> Char -> Char
encryptLetter n c
    | isLower c = chr $ (ord c + n - 97) `mod` 26 + 97
    | isUpper c = chr $ (ord c + n - 65) `mod` 26 + 65
    | otherwise = c



-- don't forget toLower and /100


crack :: [Double] -> String -> String
crack freq m = 
    let all_encrypt = [encrypt n m | n <- [0..25]]
        fr2 =  map (/100) freq
        frs = map freq_table_maker all_encrypt
    in 
        let chis =  map (`chi_calculator` fr2) frs
        in
            snd (minimumBy (\(x,y) (a,b) -> if x < a then LT else GT) $ zip chis all_encrypt ) 





    
-- --toLower cipher
-- -- group $ sort cipher
-- -- data.char ord -97
-- padGrouping :: [a] -> [a]
-- padGrouping l = foldl (\) 0 


-- frequencyTableMaker :: [a] -> String -> [Double]
-- --frequencyTableMaker groupedList length_cipher =  
-- frequencyTableMaker [] _ = []
-- frequencyTableMaker l:ls n
--     | index != length 
--     where
--         index = ord $ (head l) - 97