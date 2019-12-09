adjacentDigits :: [Int] -> Bool
adjacentDigits [] = False
adjacentDigits [x] = False
adjacentDigits (x:y:xs) = x == y || adjacentDigits ([y] ++ xs)


neverDecrease :: [Int] -> Bool
neverDecrease [] = True
neverDecrease [x] = True
neverDecrease (x:xs) = x <= (head xs) && neverDecrease xs 

isValidPassword :: Int -> Bool
isValidPassword i = neverDecrease (toIntArray i)  && adjacentDigits (toIntArray i)

splitAtDash :: String -> [String]
splitAtDash c = case dropWhile (== '-') c of
                     "" -> []
                     s' -> w : splitAtDash s''
                         where (w, s'') = break (== ',') s'

toIntArray :: Int -> [Int]
toIntArray 0 = []
toIntArray x = toIntArray (x `div` 10) ++ [x `mod` 10]

main = do
    --foo <- getLine
    --let pwRange = map (read :: String -> Int) $ splitAtDash foo
    let range = [284639..748759]
    let validPws = filter isValidPassword range
    print $ length validPws
    
