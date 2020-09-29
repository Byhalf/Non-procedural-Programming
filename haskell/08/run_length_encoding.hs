import qualified Data.ByteString.Lazy as B  
import Data.Word
import System.Environment   
-- 0 <N> <byte>
-- <N> <byte_1> <byte_2> ... <byte_N>
--pack :: [Word8] -> ByteString

decode :: [Word8] -> [Word8]
decode [] = []
decode (l:ls)
    |li == 0 = 
        let (n:ns:lss) =ls
            ni = fromIntegral n
        in  replicate ni ns ++ decode lss
    |otherwise = let (s,lss) = splitAt li ls in s++decode lss
    where li = fromIntegral l

repeated :: [Word8] -> Bool
repeated (x1:x2:x3:x4:ls) = x1==x4 && x1==x2 && x1 == x3
repeated _ = False

-- grab_repeated takes an input sequence beginning with four or more repeated bytes.
--  It returns a triple containing a repetition count (which must always be <= 255), 
--  the byte to repeat, and a sequence of bytes that follow the initial repeated bytes.

grab_repeated :: [Word8] -> (Word8, Word8, [Word8])
grab_repeated (l:ls) = grab_repeated' (0, l, l:ls) --works when not 4 repeated bits

grab_repeated' :: (Word8, Word8, [Word8]) ->  (Word8, Word8, [Word8])
grab_repeated' (255,b,l) = (255,b,l)
grab_repeated' (x,b,[l]) = if b==l then (x+1,b,[])else (x,b,[l])
grab_repeated' (x,b,(l:ls))
    |b==l= grab_repeated' (x+1,b,ls)
    |otherwise = (x,b,l:ls)


grab_unrepeated :: [Word8] -> (Word8,[Word8], [Word8]) 
grab_unrepeated bs= let (n,b,r) = grab_unrepeated' (0,[],bs) in (n, reverse b, r) 

grab_unrepeated' :: (Word8, [Word8], [Word8]) -> (Word8,[Word8], [Word8])
grab_unrepeated' (255, b, bs) = ( 255,b,bs)
grab_unrepeated' (x, b, []) = (x,b,[])
grab_unrepeated' (x, b, bs)
    |repeated bs = (x,b,bs)
    |otherwise = let (nb:nbs) = bs in grab_unrepeated' (x+1,nb:b,nbs)

encode :: [Word8] -> [Word8]
encode [] = []
encode list =
    if repeated list then
        let (count, x, rest) = grab_repeated list
        in  0:count:x: encode rest 
    else
        let (count, init, rest) = grab_unrepeated list
        in count : (init ++ encode rest)


run' :: [String] -> B.ByteString-> IO ()
run' [s] m
    | s == "-e" =  B.putStr  $ B.pack $ encode word8
    | s == "-d" =  B.putStr  $ B.pack $ decode word8
    | otherwise = run' [] m
    where word8 = B.unpack m
run' _ _= putStrLn "usage: run_length [-d | -e]"

run :: [String] -> IO()
run args = do 
    content <- B.getContents
    run' args content

main = do
    args <- getArgs
    run args 
