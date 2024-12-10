import Prelude

-- this is redundant -_-
formatContent :: String -> String
formatContent ('\n':s)  = formatContent s
formatContent (x:s)     = x : formatContent s
formatContent x         = x

-- TODO: finish helper funciton to so we can extract nums
extractMul :: String -> [(Int,Int)]
extractMul xs = go xs []
    where
        go s@(x:xs) acc 
            | take 3 s == "mul" = undefined 
            | otherwise = go xs acc

-- TODO: learn regex or smth to extract and parse the nums
getFactors :: String -> (Int, Int)
getFactors s = undefined

main :: IO()
main = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print $ formatContent contents


-- debug main
main' :: IO()
main' = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    undefined