-- 2019 Day 1 Part 2
-- 
-- Fuel needs to be calculated for Santa's sleigh. The fuel needed
-- for a given mass can be calculated with the following formula: (mass / 3) - 2
-- However, the fuel needs fuel, and that fuel needs fuel, and so on until negative fuel is needed.
-- e.g. fuel for a mass of 1969 is 654 + 216 + 70 + 21 + 5 = 966
--
-- This calculates the total amount of fuel for all of the given masses.
--
-- usage cat input.txt | ./part_2

main = do
    content <- getContents
    let contentLines = map (read :: String -> Int) (lines content)
    let fuelList = map calculateFuelForMass contentLines
    print $ sum fuelList
    
calculateFuelForMass :: Int -> Int
calculateFuelForMass mass
    | fuel <= 0 = 0
    | otherwise = fuel + calculateFuelForMass fuel
    where fuel = (mass `div` 3) - 2
