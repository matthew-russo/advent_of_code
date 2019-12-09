-- part 2 of day 2 of advent of code 2019 (https://adventofcode.com/)
-- author: Sam Berning

import System.IO

main = do
    input <- openFile "input.txt" ReadMode
    unformattedIntcode <- hGetContents input
    let intcode = strToIntcode unformattedIntcode
    putStrLn $ show $ testValues intcode

strToIntcode :: String -> [Int]
strToIntcode s = 
    if length s /= 0
        then (read (take p s) :: Int) : strToIntcode (drop (p + 1) s)
        else []
    where p = find ',' s

find :: Char -> String -> Int
find d s =
    if length s == 0 || head s == d
        then 0
        else 1 + find d (tail s)

target :: Int
target = 19690720

testValues :: [Int] -> Int 
testValues code =
    head [100 * n + v | n <- [0..99],
                        v <- [0..99],
                        (evalWithInput code n v) == target]

evalWithInput :: [Int] -> Int -> Int -> Int
evalWithInput code first second =
    head $ processOpcode (setInput code first second) 0

setInput :: [Int] -> Int -> Int -> [Int]
setInput code first second =
    update (update code first 1) second 2
    
processOpcode :: [Int] -> Int -> [Int]
processOpcode code p =
    case code !! p of
        1  -> processOpcode (runOp (+) code p) (p + 4)
        2  -> processOpcode (runOp (*) code p) (p + 4)
        99 -> code

runOp :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
runOp f code p = 
    update code result position
    where result = op1 `f` op2
          op1 = code !! (code !! (p + 1))
          op2 = code !! (code !! (p + 2))
          position = code !! (p + 3)

update :: [Int] -> Int -> Int -> [Int]
update code value p =
    before ++ [value] ++ after
    where before = take p code
          after = tail (drop p code)
