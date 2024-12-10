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

-- create a list of boolean values for each line coresponding to being safe or not
lineCheck1 :: [Int] -> Bool
lineCheck1 vs@(a:b:bs)
    | a > b     = go vs (checkNeighbours (>))
    | a < b     = go vs (checkNeighbours (<))
    | otherwise = False -- a == b violates the condition
    where
        go (x:y:ys) fun  = x `fun` y && go (y:ys) fun
        go [x] _        = True -- if there are less than 2 elements left we cant violate the condition
        go []  _        = error "reached empty list state in reportCheck (we threw away one too many elements?)"

-- helper function
removeElemAt :: Int -> [a] -> [a]
removeElemAt i [] = []
removeElemAt i ls = take i ls ++ drop (i+1) ls

-- TODO: implement reportCheck2
lineCheck2 :: [Int] -> Bool
lineCheck2 vs@(a:b:bs) 
    = lineCheck1 vs || or [ lineCheck1 y | y <- [removeElemAt i vs | i <- [0..(length vs)]]]

calculate :: ([Int] -> Bool) -> [[Int]] -> Int
calculate fun = foldr ((\x -> if x then (1 +) else (0 +)) . fun) 0

-- sum up the entire input using functions above
calculate1 :: [[Int]] -> Int
calculate1 = calculate lineCheck1

calculate2 :: [[Int]] -> Int
calculate2 = calculate lineCheck2

main :: IO()
main = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    let fContent = formatContent contents
    putStrLn "Part 1:"
    print $ calculate1 fContent
    putStrLn "Part 2:"
    print $ calculate2 fContent

--for debuging
main' :: IO()
main' = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    putStrLn "Part 1:"
    print $ map lineCheck1 $ formatContent contents
    putStrLn "Part 2:"
    print $ map lineCheck2 $ formatContent contents
