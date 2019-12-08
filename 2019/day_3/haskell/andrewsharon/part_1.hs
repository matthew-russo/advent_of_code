import Data.List (sort)

type Point = (Int, Int)

data Motion = Motion Char Int deriving Show

intersect :: [Point] -> [Point] -> [Point]
intersect [] _ = []
intersect (x:xs) ys 
    | x `elem` ys = x : intersect xs ys
    | otherwise = intersect xs ys 

distance :: Point -> Int
distance (x, y) = abs x + abs y

trace :: Point -> Motion -> [Point]
trace  (x, y) (Motion direction dist)
    | direction == 'U' = [ (x', y) | x' <- [x+1..(x+dist)] ]
    | direction == 'D' = reverse [ (x', y) | x' <- [(x-dist)..(x-1)] ]
    | direction == 'L' = reverse [ (x, y') | y' <- [(y-dist)..(y-1)] ]
    | direction == 'R' = [ (x, y') | y' <- [(y+1)..(y+dist)] ]
    | otherwise        = []

constructWire :: Point -> [Motion] -> [Point]
constructWire p [] = []
constructWire p (x:xs) = points ++ constructWire (last points) xs
    where points = trace p x 

splitByComma :: String -> [String]
splitByComma c = case dropWhile (== ',') c of
                     "" -> []
                     s' -> w : splitByComma s''
                         where (w, s'') = break (== ',') s'

parseMotion :: String -> Motion
parseMotion (d:rest) = Motion d (read rest) 

-- U7,R6,D4,L4
-- R8,U5,L5,D3
main = do
    wire1 <- getLine
    wire2 <- getLine 
    let wire1' = constructWire (0,0) $ map parseMotion $ splitByComma wire1
    let wire2' = constructWire (0,0) $ map parseMotion $ splitByComma wire2
    let intersection = intersect wire1' wire2'
    let dist = map distance intersection
    print $ head $ sort $ dist 
