-- 2019 Day 2 Part 1
-- https://adventofcode.com/2019/day/2

type Tape = ([Int], Position) 
type Position = Int

main = do
    content <- getLine
    let contentNoCommas = splitByComma content
    let contentInts = map (read :: [Char] -> Int) contentNoCommas
    print $ fst $ processOpcode (contentInts, 0) 

splitByComma :: String -> [String]
splitByComma c = case dropWhile (== ',') c of
                     "" -> []
                     s' -> w : splitByComma s''
                         where (w, s'') = break (== ',') s'

processOpcode :: Tape -> Tape
processOpcode (xs, pos)
    | opcode == 1 = processOpcode (constructNewSequence xs store (r1 + r2), (pos + 4))
    | opcode == 2 = processOpcode (constructNewSequence xs store (r1 * r2), (pos + 4))
    | otherwise = (xs, pos)
    where opcode = xs !! pos
          r1     = xs !! (xs !! (pos + 1))
          r2     = xs !! (xs !! (pos + 2))
          store  = xs !! (pos + 3)

constructNewSequence :: [Int] -> Int -> Int -> [Int]
constructNewSequence xs store val = before ++ [val] ++ after
    where (before, _)  = splitAt store xs
          (_, after)   = splitAt (store + 1) xs  

