import System.IO
import Data.Char



numFind :: [String] -> [Int]
numFind ss = numFind' ss []
    where
        numFind' :: [String] -> [Int] -> [Int]
        numFind' [s] acc = smackInts (firstNum s) (lastNum s) : acc
        numFind' (s:ss) acc = numFind' ss (smackInts (firstNum s) (lastNum s) : acc)

lastNum, firstNum :: String -> Int
lastNum [] = 0
lastNum s | isNumber(last s) = digitToInt (last s)
          | otherwise = lastNum (init s)

firstNum [] = 0
firstNum s | isNumber(head s) = digitToInt (head s)
           | otherwise = firstNum (tail s)

smackInts :: Int -> Int -> Int
smackInts x y = (x * 10) + y


inpt :: IO()
inpt = do 
    contents <- readFile "1.txt"
    let contentLines = lines contents
    let answer = sum(numFind contentLines)
    print answer
