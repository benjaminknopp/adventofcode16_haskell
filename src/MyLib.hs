module MyLib (getInput) where
import System.IO

getInput :: String -> IO String
getInput fn = do
    handle <- openFile fn ReadMode
    hGetContents handle
