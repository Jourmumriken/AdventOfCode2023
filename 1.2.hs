import System.IO
import Data.Char
import Data.List
import Data.Maybe


numsS :: [String]
numsS = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


-- I hate this >:(
stringToNum :: String -> Int
stringToNum "one" = 1
stringToNum "two" = 2
stringToNum "three" = 3
stringToNum "four" = 4
stringToNum "five" = 5
stringToNum "six" = 6
stringToNum "seven" = 7
stringToNum "eight" = 8
stringToNum "nine" = 9

-- sorry Alex <3
oneIsPrefixOf, oneIsSuffixOf :: String -> (Bool, Maybe String)
oneIsPrefixOf s = (any (`isPrefixOf` s) numsS, safeHead $ filter (`isPrefixOf` s) numsS)
oneIsSuffixOf s = (any (`isSuffixOf` s) numsS, safeHead $ filter (`isSuffixOf` s) numsS)


-- no teeth
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead as = Just (head as)



numFind :: [String] -> [Int]
numFind ss = numFind' ss []
    where
        numFind' :: [String] -> [Int] -> [Int]
        numFind' [s] acc = smackInts (firstNum s) (lastNum s) : acc
        numFind' (s:ss) acc = numFind' ss (smackInts (firstNum s) (lastNum s) : acc)



lastNum, firstNum :: String -> Int
lastNum [] = 0
lastNum s | fst (oneIsSuffixOf s) = stringToNum (fromJust (snd (oneIsSuffixOf s)))
          | isNumber (last s) = digitToInt (last s)
          | otherwise = lastNum (init s)

firstNum [] = 0
firstNum s | fst (oneIsPrefixOf s) = stringToNum (fromJust (snd (oneIsPrefixOf s)))
           | isNumber (head s) = digitToInt (head s)
           | otherwise = firstNum (tail s)

smackInts :: Int -> Int -> Int
smackInts x y = (x * 10) + y

inpt :: IO()
inpt = do
    contents <- readFile "1.txt"
    let contentLines = lines contents
    let answer = sum (numFind contentLines)
    print answer
