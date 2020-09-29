import System.FilePath
import System.Directory
import System.Environment --args
import System.IO  
import Data.List

allFilesBeneath :: FilePath -> IO [FilePath]
allFilesBeneath path = do
    isFile <- doesFileExist path
    if not isFile
        then do
            fileNames <- listDirectory path 
            let filePaths = map (combine path) (sort fileNames)
            let ioFiles = mapM (\file -> do 
                                isFile <- doesFileExist file
                                isDirectory <- doesDirectoryExist file

                                if not isDirectory
                                    then return [file]
                                    else do
                                        newFileNames <- listDirectory file
                                        let newFilePaths = map (combine file) newFileNames
                                        iONewFilePaths <- mapM allFilesBeneath newFilePaths
                                        return $ concat iONewFilePaths
                                        
                                        ) filePaths
            files <- ioFiles
            return $ concat files
    else do
        return [path]

isInLine' :: String -> String -> String -> String -> Bool
isInLine' _ [] start obtained = start == obtained
isInLine' (l:ls) (m:ms) s o
    | l == m = (isInLine' ls ms s (o ++ [l])) || (isInLine' ls  s s "")
    | l /= m = isInLine' ls s s ""
isInLine' [] _ e r= e==r

isInLine :: String -> String -> Bool
isInLine l m = isInLine' l m m ""


findLine :: FilePath -> String -> IO String
findLine match file  = do
                        contents <- readFile file
                        let res = filter (`isInLine` match) (lines contents)
                        let toadd = file ++ ": " 
                        return $ unlines (map ((++) toadd) res)

--sort by length of file paths
--sortBy (\x y -> if length (splitPath x)> length (splitPath y) then GT else LT)


run :: [String] -> IO ()
run [string,path] = do 
                        allFiles <- allFilesBeneath path
                        --mapM putStrLn allFiles
                        let sortAllFiles =  allFiles
                        nearlyRes <- mapM (findLine string) sortAllFiles
                        mapM putStr (filter (/= []) nearlyRes)
                        return ()
run _ = do 
            putStrLn "usage: find_in_files <string> <dir>"


main = do 
    args <- getArgs
    run args
