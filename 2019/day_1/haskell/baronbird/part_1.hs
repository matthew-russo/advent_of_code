-- part 1 of day 1 of advent of code 2019 (https://adventofcode.com/)
-- author: Sam Berning

import System.IO

main = do
    input <- openFile "input.txt" ReadMode
    contents <- hGetContents input
    putStrLn $ show $ sumFuels contents

sumFuels contents = 
    sum [getFuel (read line :: Double) | line <- lines contents]

getFuel :: Double -> Int
getFuel m = floor (m / 3) - 2
