module Main where

import Data.Char ( isAlpha, toLower ) 
import Data.Functor ( (<&>) )
import Data.List ( sortBy )
import Data.Function ( on )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import Control.Monad ( filterM, forM, forM_ )

import qualified System.Directory as D
import qualified Data.Map.Strict as M

-- Получение всех файлов (имена) из директории и вложенных поддирккторий
getAllFiles :: FilePath -> IO [FilePath]
getAllFiles path = do
    allFiles <- D.listDirectory path >>=  mapM (return . (path </>))

    subDirectories <- filterM D.doesDirectoryExist allFiles
    directoryFiles <- filterM D.doesFileExist allFiles

    subDirectoryFiles <- mapM getAllFiles subDirectories <&> concat

    return $ directoryFiles ++ subDirectoryFiles

-- Разбивает предложение на слова, оставив только буквы
splitWords :: String -> [String]
splitWords s = filter (not . null) $ 
                    map (filter isAlpha) $ 
                    words s 

-- превратим список с список пар со сдвигом 1
listToListOfPairs :: [a] -> [(a, a)]
listToListOfPairs lst = zip lst (tail lst)

-- получает содержимое файла, исключая слова
getFileContent :: FilePath -> [String] -> IO [String]
getFileContent path extraWords = do
    content <- readFile path
    let splitted = splitWords content
        lowered = map (map toLower) splitted
        filtred = filter (not . flip elem extraWords) lowered
        in return filtred

-- функция map из MapReduce
mapper :: FilePath -> (FilePath -> IO [String]) -> IO [((String, String), Int)]
mapper path getContent = do
    content <- getContent path
    let pairs = listToListOfPairs content
        in return $ zip pairs (repeat 1)

-- функция reduce
reducer :: [[((String, String), Int)]] -> [((String, String), Int)]
reducer lst = do 
    let concated = concat lst
        f = \(k, v) m -> M.insertWith (+) k v m
        reduced = M.toList $ foldr f M.empty concated
        in sortBy (flip compare `on` snd) reduced

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 
        then putStrLn "incorrect args count"
    else do
        extraWords <- readFile (head args) <&> words
        existingFiles <- filterM D.doesDirectoryExist (tail args)
        allFiles <- mapM getAllFiles existingFiles <&> concat
        mapped <- forM allFiles $ flip mapper $ flip getFileContent extraWords
        forM_ (reducer mapped) print