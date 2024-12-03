import Prelude
import Data.List (sort)
import GHC.IO.FD (openFile)
import GHC.IO.Handle.Types (Handle__)


-- a bit dumb but its nice to make sure I dont use the wrong function (like lines) :)
splitContent :: String -> [String]
splitContent = words

formatContent :: [String] -> ([Int],[Int])
formatContent xs = go xs ([],[])
    where
        go [] (l,r)       = (l,r)
        go (x:y:ys) (l,r) = go ys ((read x :: Int):l,(read y :: Int):r)
        go _ _            = error "odd list lenths in formatContent?"

sortLists :: ([Int],[Int]) -> ([Int],[Int])
sortLists (l,r) = (sort l, sort r)

calculate :: ([Int],[Int]) -> Int
calculate ([l],[r])     = abs (l - r)
calculate (l:ls,r:rs)   = abs (l - r) + calculate (ls,rs)
calculate _             = error "reached wildcared case in calculate"

occurancesOf :: Eq a => a -> [a] -> Int
occurancesOf x xs = length $ filter (x ==) xs

calculate1 :: ([Int],[Int]) -> Int
calculate1 ([l],r)      = l * occurancesOf l r
calculate1 (l:ls,r)     = (l * occurancesOf l r) + calculate1 (ls,r)
calculate1 _            = error "reached wildcard case in calculate1"

main :: IO()
main = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print (calculate $ sortLists $ formatContent $ splitContent contents)
    putStrLn "\nAnd similarity score is:"
    print (calculate1 $ sortLists $ formatContent $ splitContent contents)

-- debug main
main' :: IO()
main' = do
    putStrLn "Give me a file path:"
    filePath <- getLine
    contents <- readFile filePath
    print (calculate $ sortLists $ formatContent $ splitContent contents)
    putStrLn "\nAnd similarity score is:"
    print (calculate1 $ sortLists $ formatContent $ splitContent contents)
