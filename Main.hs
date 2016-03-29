import Control.Applicative
import Data.Char (toLower)
import Data.List.Split
import System.IO
import System.Random

main :: IO ()

main = do
    (x, y) <- getWord
    putStrLn y
    hFlush stdout
    line <- getLine
    if line /= "exit"
        then
            checkAns line x
        else
            return ()
    return ()

checkAns :: String -> String -> IO ()

checkAns line ans = do
    if map toLower line == map toLower ans
        then putStrLn "Correct.\n"
        else putStrLn $ "Incorrect, correct answer was '" ++ ans ++ "'.\n"
    main


getWord :: IO (String, String)

getWord = do
    list <- fileStuff

    let len = length list
    
    index <- randomRIO (0, len - 1)

    return $ list !! index


fileStuff :: IO [(String, String)]

fileStuff = do
    contents <- lines <$> readFile "C:/Users/Lewis/Documents/Languages/Norwegian/Vanskelige Norske Ord2.mdown"

    let x = map (head . splitOn " - ") contents
        y = map (last . splitOn " - ") contents

    return $ zip x y