import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- all the digits as strings
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

-- find the first and last number using help functions
numFind2 :: [String] -> [Int]
numFind2 ss = numFind' ss []
    where
        numFind' :: [String] -> [Int] -> [Int]
        numFind' [s] acc = smackInts (firstNum2 s) (lastNum2 s) : acc
        numFind' (s:ss) acc = numFind' ss (smackInts (firstNum2 s) (lastNum2 s) : acc)

numFind1 :: [String] -> [Int]
numFind1 ss = numFind' ss []
    where
        numFind' :: [String] -> [Int] -> [Int]
        numFind' [s] acc = smackInts (firstNum1 s) (lastNum1 s) : acc
        numFind' (s:ss) acc = numFind' ss (smackInts (firstNum1 s) (lastNum1 s) : acc)


-- said help functions
lastNum2, firstNum2 :: String -> Int
lastNum2 [] = 0
lastNum2 s | fst (oneIsSuffixOf s) = stringToNum (fromJust (snd (oneIsSuffixOf s)))
          | isNumber (last s) = digitToInt (last s)
          | otherwise = lastNum2 (init s)

firstNum2 [] = 0
firstNum2 s | fst (oneIsPrefixOf s) = stringToNum (fromJust (snd (oneIsPrefixOf s)))
           | isNumber (head s) = digitToInt (head s)
           | otherwise = firstNum2 (tail s)

lastNum1, firstNum1 :: String -> Int
lastNum1 [] = 0
lastNum1 s | isNumber(last s) = digitToInt (last s)
          | otherwise = lastNum1 (init s)

firstNum1 [] = 0
firstNum1 s | isNumber(head s) = digitToInt (head s)
           | otherwise = firstNum1 (tail s)



-- smack two digits together to make one number
smackInts :: Int -> Int -> Int
smackInts x y = (x * 10) + y

-- main function!
main :: IO()
main = do
    contents <- readFile "1.txt"
    let contentLines = lines contents
    let answer1 = sum (numFind1 contentLines)
    let answer2 = sum (numFind2 contentLines)
    putStrLn $ "answer 1: " ++ show answer1 ++ "\nanswer 2: " ++ show answer2  
