# 2019 Day 1 Part 1
# 
# Fuel needs to be calculated for Santa's sleigh. The fuel needed
# for a given mass can be calculated with the following formula: (mass / 3) - 2
# 
# This calculates the total amount of fuel for all of the given masses.
#
# usage cat input.txt | ./part_1

main = do
    content <- getContents
    let contentLines = map (read :: String -> Int) (lines content)
    let fuelList = map calculateFuelForMass contentLines
    print $ sum fuelList
    
calculateFuelForMass :: Int -> Int
calculateFuelForMass mass = (mass `div` 3) - 2
