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
reportCheck1 :: [Int] -> Bool
reportCheck1 vs@(a:b:bs)
    | a > b     = go vs (checkNeighbours (>))
    | a < b     = go vs (checkNeighbours (<))
    | otherwise = False -- a == b violates the condition
    where
        go (x:y:ys) fun  = x `fun` y && go (y:ys) fun
        go [x] _        = True -- if there are less than 2 elements left we cant violate the condition
        go []  _        = error "reached empty list state in reportCheck (we threw away one too many elements?)"

-- TODO: implement reportCheck2
reportCheck2 :: [Int] -> Bool
reportCheck2 vs@(a:b:bs) = undefined

-- sum up the entire input using functions above
calculate1 :: [[Int]] -> Int
calculate1 = calculate reportCheck1

calculate2 :: [[Int]] -> Int
calculate2 = calculate reportCheck2

calculate :: ([Int] -> Bool) -> [[Int]] -> Int
calculate fun = foldr ((\x -> if x then (1 +) else (0 +)) . fun) 0

main :: IO()
main = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print $ calculate1 $ formatContent contents


--for debuging
main' :: IO()
main' = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    putStrLn "Part 1:"
    print $ map reportCheck1 $ formatContent contents
    putStrLn "Part 2:"
    print $ map reportCheck2 $ formatContent contents
