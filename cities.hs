--
-- MATHFUN
-- UP2058177
--

import Text.Printf (printf)
import Data.List (find, sortOn)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import System.Directory (doesFileExist)

--
-- Types
--

type CityName = String
type Latitude = Int
type Longitude = Int 
type PopData = [Int]

data Location = Location Latitude Longitude
    deriving (Eq,Ord,Show,Read)

data City = City CityName Location PopData 
    deriving (Eq,Ord,Show,Read)

--
-- Test Data
--

testData :: [City]
testData = 
    [ City "Amsterdam" (Location 52 5) [1158, 1149, 1140, 1132],
      City "Athens" (Location 38 24) [3153, 3153, 3154, 3156],
      City "Berlin" (Location 53 13) [3567, 3562, 3557, 3552],
      City "Bucharest" (Location 44 26) [1794, 1803, 1812, 1821],
      City "London" (Location 52 0) [9426, 9304, 9177, 9046],
      City "Madrid" (Location 40 (-4)) [6669, 6618, 6559, 6497],
      City "Paris" (Location 49 2) [11079, 11017, 10958, 10901],
      City "Rome" (Location 42 13) [4278, 4257, 4234, 4210],
      City "Vienna" (Location 48 16) [1945, 1930, 1915, 1901],
      City "Warsaw" (Location 52 21) [1790, 1783, 1776, 1768]
    ]

--
--  Your functional code goes here
--

--
-- Helper functions
--

formatPopulation :: Int -> String
formatPopulation pop = printf "%.3f" (fromIntegral pop / 1000 :: Float) ++ "m"

calcDistance :: (Int, Int) -> (Int, Int) -> Float
calcDistance (lat1, long1) (lat2, long2) = sqrt (fromIntegral (lat2 - lat1)^2 + fromIntegral (long2 - long1)^2)

--
-- Main code
--

-- (i) Return a list of the names of all the cities
listAllCities :: [City] -> [String]
listAllCities [] = []
listAllCities (City name _ _ : xs) = name : listAllCities xs

-- (ii) Return the population of the city that number of years ago
getPopYearsAgo :: [City] -> CityName -> Int -> String
getPopYearsAgo [] _ _ = "no data"
getPopYearsAgo ((City name _ list) : xs) city num
    | name /= city = getPopYearsAgo xs city num
    | num > length list - 1 || num < 0 = "no data"
    | otherwise = formatPopulation (list !! num) 

-- (iii) Return all the data as a single string which, when output using putStr, will display the data formatted neatly into five columns
formatCity :: City -> String
formatCity (City name (Location latVal longVal) list) =
    printf "%-15s %-10s %-12s %-21s %0s"
    name (show latVal ++ "° N") longitude (getPopYearsAgo [City name (Location latVal longVal) list] name 0) (getPopYearsAgo [City name (Location latVal longVal) list] name 1)
    where
        longitude = if longVal >= 0 then show longVal ++ "° E" else show (longVal * (-1)) ++ "° W"

formatData :: [City] -> String
formatData [] = ""
formatData (x : xs) = formatCity x ++ "\n" ++ formatData xs 

printData :: [City] -> String
printData list = 
    printf "%-15s %-10s %-12s %-21s %0s"
    "City" "Latitude" "Longitude" "Current Population" "Last Year's Population"
    ++ "\n" ++ "-------------------------------------------------------------------------------------" ++ "\n" ++ formatData list

-- (iv) Update the data with a list of new population figures 
updateCity :: City -> Int -> City
updateCity (City name (Location latVal longVal) list) num = City name (Location latVal longVal) (num : list)

updatePopData :: [City] -> [Int] -> [City]
updatePopData [] _ = []
updatePopData (x : xs) (y : ys) = if length (y : ys) /= length (x : xs) then x : xs else updateCity x y : updatePopData xs ys 

-- (v) Add a new city to the list
addCity :: [City] -> CityName -> Latitude -> Longitude -> PopData -> [City]
addCity cities newCity newLat newLong newList = sortOn (\(City name _ _) -> name) (cities ++ [City newCity (Location newLat newLong) newList])

-- (vi) For a given city name, return a list of annual population growth figures for that city
getAnnualPopGrowth :: [City] -> CityName -> [Int]
getAnnualPopGrowth [] _ = []
getAnnualPopGrowth ((City name _ list) : xs) city = if name == city then zipWith (-) list (tail list) else getAnnualPopGrowth xs city 

-- (vii) Given a location and a number, return the name of the closest city with a population bigger than the number
findClosestCity :: [City] -> Location -> Int -> String
findClosestCity [] _ _ = "no city"
findClosestCity cities (Location lat2 long2) num = (fst . head) (sortOn snd [(name, calcDistance (lat1, long1) (lat2, long2)) | (City name (Location lat1 long1) list) <- cities])

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = putStrLn (show (listAllCities testData) :: String) -- output the names of all the cities
demo 2 = putStrLn (show (getPopYearsAgo testData "Berlin" 1) :: String) -- output the population of "Berlin" 1 year ago (i.e. last year)
demo 3 = putStrLn (printData testData)
demo 4 = putStrLn (printData (updatePopData testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))
         -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
demo 5 = putStrLn (printData (addCity testData "Stockholm" 59 18 [1657, 1633, 1608, 1583]))
         -- show the data (as for (iii)) after adding "Stockholm" (59N, 18E) 
         -- with population figures [1657, 1633, 1608, 1583]
demo 6 = putStrLn (show (getAnnualPopGrowth testData "Athens") :: String) -- output a list of annual growth figures for "Athens"
demo 7 = putStrLn (show (findClosestCity testData (Location 45 8) 4) :: String)
         -- output the nearest city to location (45N, 8E) with 
         -- a population above 4m people
demo 8 = mapCities testData -- output the population map


--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

-- Draws horizontal border
drawHorizontal :: Int -> IO ()
drawHorizontal x = do
    if x >= 0
        then do
            writeAt (80 - x, 0) ("\x1b[33m" ++ "-")
            writeAt (80 - x, 50) "-"
            drawHorizontal (x - 1)
        else 
            return ()

-- Draws vertical border
drawVertical :: Int -> IO ()
drawVertical y = do
    if y >= 0
        then do
            writeAt (0, 50 - y) ("\x1b[33m" ++ "|")
            writeAt (80, 50 - y) "|"
            drawVertical (y - 1)
        else 
            return ()

--
-- Your population map code goes here
--

plotCities :: [City] -> IO ()
plotCities [] = return ()
plotCities ((City c1 (Location n1 e1) (y : _)) : xs) = do
    let translatedEast =  round ((80 / 70) *  fromIntegral (25 + e1) :: Float)
    let translatedNorth = round ((50 / 38) * fromIntegral (72 - n1) :: Float)
    writeAt (translatedEast, translatedNorth) ("\x1b[31m" ++ "+")
    writeAt (translatedEast + 1, translatedNorth + 1) ("\x1b[32m" ++ c1 ++ "\x1b[0m")
    writeAt (translatedEast + 1, translatedNorth + 2) (formatPopulation y)
    plotCities xs

mapCities :: [City] -> IO ()
mapCities xs = do
    clearScreen
    drawHorizontal 80
    drawVertical 50
    plotCities xs
    goTo(0,50)
    putStr "\n"

--
-- Your user interface (and loading/saving) code goes here
--

--
-- UI helper functions
--

validCoord :: Location -> Bool
validCoord (Location x y)
    | x > 72 || x < 34 = False
    | y > 45 || y < (-25) = False
    | otherwise = True 

checkStrIsNum :: String -> Bool
checkStrIsNum = all isDigit

checkEachElement :: String -> Bool
checkEachElement [] = True
checkEachElement (x : xs) = if x /= '[' && x /= ',' && x /= ']' then checkStrIsNum [x] && checkEachElement xs else checkEachElement xs

checkList :: String -> Bool
checkList list = head list == '[' && last list == ']' && checkEachElement list

toCity :: [String] -> [City]
toCity = map (\x -> read x :: City)

formatInput :: String -> IO String
formatInput prompt = do
    putStr prompt
    getLine

displayMenu :: IO ()
displayMenu = do
  putStrLn "Select an option:"
  putStrLn "1. List all cities"
  putStrLn "2. Return population of a city x years ago"
  putStrLn "3. Return all the data"
  putStrLn "4. Update the data with a list of new populations"
  putStrLn "5. Add a new city to the list"
  putStrLn "6. Annual population growth for a city"
  putStrLn "7. Find closest city with at least x population to co-ordinate y"
  putStrLn "8. Output population map (will display on an 80 char, 50 line map)"
  putStrLn "Any other key to exit..."
  putStr "\n"

--
-- User interface code
--

loop :: [City] -> IO ()
loop cities = do
    putStrLn "---------------------------------------------------------------------------------------------------------------------------"
    displayMenu
    choice <- formatInput "Enter a number to begin: "
    case choice of 

        "1" -> do 
            putStrLn (show (listAllCities cities) :: String)
            loop cities

        "2" -> do
            city <- formatInput "Enter the city name: "
            year <- formatInput "How many years ago? (Press 0 to view the current population): "
            putStrLn (show (getPopYearsAgo cities city (read year)) :: String)
            loop cities

        "3" -> do 
            putStrLn (printData cities)
            loop cities

        "4" -> do
            newPopulation <- formatInput "Enter a list of new population data (There must be an entry for each city): "
            if checkList newPopulation then do
                let newCities = updatePopData cities (read newPopulation)
                if newCities == cities then do
                    putStrLn "ERROR: Length of list does not match number of cities"
                    loop cities
                else do
                    putStrLn (printData newCities)
                    loop newCities
            else do
                putStrLn "ERROR: Parameters are of wrong type"
                loop cities

        "5" -> do
            cityName <- formatInput "Enter the name of the city: "
            lat <- formatInput "Enter the latitude in North degrees: "
            long <- formatInput "Enter the longitude in East degrees: "
            popData <- formatInput "Enter a list of population data (must be as long as the lists for existing cities): "
            if checkStrIsNum lat && checkStrIsNum long && checkList popData then do
                if validCoord (Location (read lat) (read long)) then do
                    let newCities = addCity cities cityName (read lat) (read long) (read popData)
                    if newCities == cities then do
                        putStrLn "ERROR: Length of list does not match length of existing population lists"
                        loop cities
                    else do
                        putStrLn (printData newCities)
                        loop newCities
                else do
                    putStrLn "ERROR: Co-ordinates must be within Europe"
                    loop cities
            else do
                putStrLn "ERROR: Parameters are of wrong type"
                loop cities

        "6" -> do
            cityName <- formatInput "Enter the name of the city: "
            if isJust (find (==cityName) (listAllCities cities)) then do
                putStrLn (show (getAnnualPopGrowth cities cityName) :: String)
            else do
                putStrLn "ERROR: City does not exist"
            loop cities

        "7" -> do
            lat <- formatInput "Enter the latitude in North degrees of the co-ordinate: "
            long <- formatInput "Enter the longitude in East degrees of the co-ordinate: "
            population <- formatInput "Enter the population (in millions and as an integer): "
            if checkStrIsNum lat && checkStrIsNum long && checkStrIsNum population then do
                let location = Location (read lat) (read long)
                if validCoord location then do
                    putStrLn (show (findClosestCity cities location (read population)) :: String)
                else do
                    putStrLn "ERROR: Co-ordinates must be within Europe"
            else do
                putStrLn "ERROR: Parameters are of wrong type"
            loop cities

        "8" -> do 
            mapCities cities
            loop cities

        _ -> do
            writeFile "cities.txt" (unlines (map show cities))

main :: IO ()
main = do
    putStr "\n"
    x <- doesFileExist "cities.txt"
    if x then do
        contents <- readFile "cities.txt"
        let cities = toCity (lines contents)
        putStrLn (show (listAllCities cities) :: String)
        loop cities
    else do
        putStrLn "ERROR: File does not exist"