import Prelude

-- format the input into a list of reports (who are themselves lists of integers)
formatContent :: String -> [[Int]]
formatContent c = map formatLine $ lines c

-- help function that formats each line of the input (report) and parses each digit into an int
formatLine :: String -> [Int]
formatLine = map (\x -> read x ::Int) . words

-- check if any two numbers break the invariant given some comparator operator
checkNeighbours :: (Int -> Int -> Bool) -> Int -> Int -> Bool
checkNeighbours op x y  =
    x `op` y && abs (x - y) < 4 -- are we increasing or decreasing too much?
--     ^ is it inc-/decrementing?

-- create a list of boolean values for each report coresponding to being safe or not
reportCheck :: [Int] -> Bool
reportCheck vs@(a:b:bs)
    | a > b     = go vs (checkNeighbours (>))
    | a < b     = go vs (checkNeighbours (<))
    | otherwise = False -- a == b violates the condition
    where
        go (x:y:ys) fun  = x `fun` y && go (y:ys) fun
        go [x] _        = True -- if there are less than 2 elements left we cant violate the condition
        go []  _        = error "reached empty list state in reportCheck (we threw away one too many elements?)"

-- sum up the entire input using functions above
calculate :: [[Int]] -> Int
calculate = foldr ((\x -> if x then (1 +) else (0 +)) . reportCheck) 0

main :: IO()
main = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print $ calculate $ formatContent contents


--for debuging
main' :: IO()
main' = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print $ map reportCheck $ formatContent contents
