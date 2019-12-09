-- 2019 Day 2 Part 1
-- https://adventofcode.com/2019/day/2

import Control.Monad

type Tape = ([Int], Position) 
type Position = Int

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

replaceInts :: [Int] -> Int -> Int -> [Int]
replaceInts src noun verb = [(head src)] ++ [noun, verb] ++ (drop 3 src)

flatten :: [[a]] -> [a] 
flatten [] = []
flatten [[a]] = [a]
flatten (x:xs) = x ++ flatten xs

main = do
    content <- getLine
    let contentNoCommas = splitByComma content
    let contentInts = map (read :: [Char] -> Int) contentNoCommas
    allResults <- forM [0..99] $ \noun -> do
        forM [0..99] $ \verb -> do
            let replacedInts = replaceInts contentInts noun verb
            let tape = processOpcode (replacedInts, 0)
            let first = head $ fst tape 
            return (first, noun, verb)
    let res = filter (\(first, noun, verb) -> first == 19690720) $ flatten allResults
    print $ head res 
    
