adjacentDigits :: [Int] -> Bool
adjacentDigits [a, b, c, d, e, f]
  | (a == b) && (a /= c) = True
  | (b == c) && (a /= b) && (c /= d) = True
  | (c == d) && (b /= c) && (d /= e) = True
  | (d == e) && (c /= d) && (e /= f) = True
  | (e == f) && (d /= e) = True
  | otherwise = False
adjacentDigits _ = False

neverDecrease :: [Int] -> Bool
neverDecrease [] = True
neverDecrease [x] = True
neverDecrease (x:xs) = x <= (head xs) && neverDecrease xs 

isValidPassword :: Int -> Bool
isValidPassword i = neverDecrease (toIntArray i)  && adjacentDigits (toIntArray i)

toIntArray :: Int -> [Int]
toIntArray 0 = []
toIntArray x = toIntArray (x `div` 10) ++ [x `mod` 10]

main = do
    let range = [284639..748759]
    let validPws = filter isValidPassword range
    print $ length validPws
    
