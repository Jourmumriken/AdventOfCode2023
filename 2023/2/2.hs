import Data.Char

-- harcoded max values for each colour
maxRed, maxBlue, maxGreen :: Int
maxRed = 12
maxBlue = 14
maxGreen = 13

-- all colours as strings
colours :: [String]
colours = ["red", "blue", "green"]


-- extract the number of all games
gameNum :: String -> (Int, [String])
gameNum s = let s' = tokenise s in (read (s' !! 1), drop 3 s')

-- tokenise a string
tokenise :: String -> [String]
tokenise s = tokenise' s [] []
    where
        -- here be dragons
        tokenise' [] acc stk = reverse $ map reverse $ filter (/= "") (stk : acc)
        tokenise' (s:ss) acc stk =
            case s of
                -- sorry alex
                ':' -> tokenise' ss (":":stk:acc) []
                ';' -> tokenise' ss (";":stk:acc) []
                ',' -> tokenise' ss (",":stk:acc) []
                ' ' -> tokenise' ss (stk : acc)   []
                _   -> tokenise' ss acc           (s:stk)


-- make the token list into touples with values and colours
makeTouples :: (Int, [String]) -> (Int, [[(String, Int)]])
makeTouples (n,ss) = (n,makeTouple ss)
-- se func above
makeTouple :: [String] -> [[(String, Int)]]
makeTouple ss = filter (/= []) (makeTouple' ss [] [] [])
    where
        makeTouple' :: [String] -> [[(String, Int)]] -> [String] -> [(String,Int)]-> [[(String, Int)]]
        makeTouple' []     acc stk stk2 = ((head stk, read (stk !! 1)) : stk2) : acc
        makeTouple' (s:ss) acc stk stk2 = case s of
            "," -> makeTouple' ss acc [] ((head stk, read (stk !! 1)):stk2)
            ";" -> makeTouple' ss (((head stk, read (stk !! 1)):stk2):acc) [] []
            _   -> makeTouple' ss acc (s:stk) stk2



-- return all games that are possible
coolGames :: [(Int,[[(String, Int)]])] -> [Int]
coolGames l = [n | (n,rs) <- filter (gameWorks.snd) l]

-- dont touch!
gameWorks :: [[(String, Int)]] -> Bool
gameWorks = all (all f)
    where
        f :: (String, Int) -> Bool
        f (s,n) = case s of
            "red"   -> n <= maxRed
            "green" -> n <= maxGreen
            "blue"  -> n <= maxBlue


minCubes :: (Int, [[(String, Int)]]) -> (Int, [(String, Int)])
minCubes (n,rs) = (n, func hList 0 0 0)
    where
        hList = map minCubesRound rs

        func :: [[(String, Int)]] -> Int -> Int -> Int -> [(String,Int)]
        func [] r g b     = [("red",r),("green",g),("blue",b)]
        func ([("red",rv),("green",gv),("blue",bv)] : xs) r g b = func xs (max r rv) (max g gv) (max b bv)
        func _ _ _ _ = error "what the fuck minCubes"

-- minimum cubes for any given round
minCubesRound :: [(String, Int)] -> [(String, Int)]
minCubesRound x = minCubesSmol' x 0 0 0
    where
        minCubesSmol' [] r g b = [("red",r),("green",g),("blue",b)]
        minCubesSmol' ((s,v):xs) r g b
            -- sorry alex
            | s == "red"   && r < v = minCubesSmol' xs v g b
            | s == "green" && g < v = minCubesSmol' xs r v b
            | s == "blue"  && b < v = minCubesSmol' xs r g v


power :: (Int, [(String, Int)]) -> Int
power (n,[(_,r),(_,g),(_,b)]) = r * b * g

-- main function
main :: IO()
main = do
    contents <- readFile "2.txt"
    let sumGames = sum $ coolGames $ map (makeTouples . gameNum) $ lines contents
    let x = sum $ map (power . minCubes . makeTouples . gameNum) $ lines contents
    putStrLn ("part 1: " ++ show sumGames ++ "\npart 2: " ++ show x)

