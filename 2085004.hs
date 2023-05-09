-----------------------------------------------------------------------------------------------------------------------------------

------------------------------- Importing Modules --------------------------------

import Data.List (groupBy, sortOn, maximumBy, minimumBy, intercalate, find)
import Text.Read (readMaybe) --allows us to safely convert from a String to another data type
import Text.Printf (printf) -- allows us to format string and output it
import System.Directory --We use it to work with files and directories 


-------------------------------------------------------------------------------------------------------------------------------------

----------- Define a custom daty type named City. ----------------------------------- 
----------- Fields: cityName, cityLongitude, cityLatitude, cityPopulation -----------
----------- Field Types: String, Float, Float, List of Floats -----------------------

-- Data type for City
data City = City { cityName :: String
                 , cityLongitude :: Float
                 , cityLatitude :: Float
                 , cityPopulation :: [Float]
                 } deriving (Show, Read) --deriving keyword is used to automatically create instances of the Show and Read type classes.

-- Show: convert any type value to String , Read: convert Strings to any type value 


--------------------------------------------------------------------------------------------------------------------------------------


-- Defining testData list with the values for 10 cities (cityName, cityLongitute, cityLatitute, cityPopulation)
testData :: [City]
testData =
  [ City "Amsterdam"    52   5     [1158, 1149, 1140, 1132],  
    City "Athens"       38  24     [3153, 3153, 3154, 3156],    
    City "Berlin"       53  13     [3567, 3562, 3557, 3552],
    City "Bucharest"    44  26     [1794, 1803, 1812, 1821],
    City "London"       52   0     [9426, 9304, 9177, 9046],
    City "Madrid"       40   4     [6669, 6618, 6559, 6497],
    City "Paris"        49   2     [11079, 11017, 10958, 10901],
    City "Rome"         42  13     [4278, 4257, 4234, 4210],
    City "Vienna"       48  16     [1945, 1930, 1915, 1901],
    City "Warsaw"       52  21     [1790, 1783, 1776, 1768]
  ]




-------------------------------------------------------------------------------------------------------------------------------------




-- Defining the demo function to show the basic program functions. Based on the template.hs given to us.
demo :: Int -> IO ()
demo 1 = putStrLn "Names of all the Cities:" >> (putStrLn . unlines . map cityName $ sortOn cityName testData)
demo 2 = putStrLn "Population of Berlin 1 year ago (last year):" >> print (formatPopulation (cityPopulationLastYear berlinCity))
  where berlinCity = head $ filter (\city -> cityName city == "Berlin") testData
demo 3 = putStrLn (citiesToString testData)
demo 4 = putStrLn "Updated city data with last year's population:"
         >> print (updateCityData testData [1200, 3200, 3600, 1800, 9500, 6800, 11100, 4300, 2000, 1800])
demo 5 = addCity testData >>= \updatedCities -> do
  putStrLn "All city data formatted neatly after adding Stockholm:"
  putStrLn (formatCityData updatedCities)
demo 6 = do
  let athensCity = head $ filter (\city -> cityName city == "Athens") testData
  let populationData = cityPopulation athensCity
  let growthFigures = zipWith (\prev curr -> curr - prev) populationData (tail populationData)
  putStrLn "Annual growth figures for Athens:"
  putStrLn (intercalate ", " $ map show growthFigures)
demo 7 = do
  let targetLatitude = 45
  let targetLongitude = 8
  let filteredCities = filter (\city -> cityLatitude city == targetLatitude &&
                                        cityLongitude city == targetLongitude &&
                                        cityPopulation city !! 0 > 4e6) testData
  let nearestCity = minimumBy (\city1 city2 -> compare (cityDistance city1 targetLatitude targetLongitude) 
                                                       (cityDistance city2 targetLatitude targetLongitude)) filteredCities
  putStrLn "Nearest city to location (45N, 8E) with population above 4 million:"
  putStrLn (cityName nearestCity)
demo 8 = putStrLn "City map:" >> printCityMap testData
demo _ = putStrLn "Invalid demo number"



------------------------------------------------------------------------------------------------------------------------------------------------



--Takes a List of Floats (Populations), as an input and return a formatted string, as millions
formatPopulation :: [Float] -> String
formatPopulation [] = "no data"
formatPopulation xs = intercalate ", " (map formatFloat xs) ++ "m"
  where formatFloat x = printf "%.3f" (x / 1000000)


--Takes a List of 'City'-ies, and returns a formatted string representatios of said cities.
--uses a local Function 'formatCity' that takes a 'City' object and formats it.
--uses a local Function 'formatPopulation' that takes a list of floats (populations) and formats it.
formatCityData :: [City] -> String
formatCityData cities =
  let
    formatCity city = cityName city ++ "\t" ++ show (cityLatitude city) ++ " N, " ++ show (cityLongitude city) ++ " E\t" ++ formatPopulation (cityPopulation city) ++ "\t" ++ formatPopulationLastYear city
    formatPopulation [] = "no data"
    formatPopulation (x:xs) = intercalate ", " (map formatFloat (x:xs)) ++ "m"
    formatFloat x = printf "%.3f" (x / 1000000)
    formatPopulationLastYear = formatPopulation . take 1 . reverse . cityPopulation
  in
    unlines $ map formatCity cities



--Takes a 'City' argument and gives (return), a string that has been format based on the 'formatCityData'.
formatCity :: City -> String
formatCity city = 
  cityName city ++ "\t" ++
  show (cityLatitude city) ++ " N, " ++
  show (cityLongitude city) ++ " E\t" ++
  formatPopulation (cityPopulation city) ++ "\t" ++
  formatPopulationLastYear city
  where
    formatPopulation [] = "no data"
    formatPopulation xs = intercalate ", " (map formatFloat xs) ++ "m"
    formatFloat x = printf "%.3f" (x / 1000000)
    formatPopulationLastYear = formatPopulation . take 1 . reverse . cityPopulation



--Gets a Float as input, and formats it as a string
-- ' "%.3f" ' , is a format specifier that makes it have 3 decimal places.
formatFloat :: Float -> String
formatFloat x = printf "%.3f" x

--Converts degrees int radians
degToRad :: Float -> Float
degToRad deg = deg * (pi / 180)

--Earth's radius constant in km
earthRadius :: Float
earthRadius = 6371.0

--Calculateing the population of a city for the last year (calculation assums that the population has linear growth
cityPopulationLastYear :: City -> [Float]
cityPopulationLastYear city = [last (cityPopulation city)]



--Takes as input two lists (a list of cities, and a list of floats (for the population)), and then it returns a new cities list where population is updated
updateCityData :: [City] -> [Float] -> [City]
updateCityData [] _ = []
updateCityData _ [] = []
updateCityData (c:cs) (p:ps) = updatedCity : updateCityData cs ps
  where updatedCity = City { cityName = cityName c
                           , cityLongitude = cityLongitude c
                           , cityLatitude = cityLatitude c
                           , cityPopulation = (cityPopulation c) ++ [p]
                           }


--Calculating the distance between two cities in km, according to longitude and latitude
--Haversine formula is applied to longitute and latitude to finde the difference between two points
cityDistance :: City -> Float -> Float -> Float
cityDistance city targetLatitude targetLongitude =
  let lat1 = cityLatitude city
      lon1 = cityLongitude city
      lat2 = targetLatitude
      lon2 = targetLongitude
      dLat = degToRad (lat2 - lat1)
      dLon = degToRad (lon2 - lon1)
      haversineFormula = sin (dLat / 2) * sin (dLat / 2) + cos (degToRad lat1) * cos (degToRad lat2) * sin (dLon / 2) * sin (dLon / 2)
      calculatingDistance = 2 * atan2 (sqrt haversineFormula) (sqrt (1 - haversineFormula))
      distance = earthRadius * calculatingDistance
  in distance


--Gets an integer 'n' and a list 'xs' as input, gives us a list of lists. Each list contains 'n' elements of the og list
-- ** if xs mod(n) != 0 , then the last inner list has <n elements. 
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


--Takes a float as input, formats it to two decimal places, and then returns it to us
formatNumber :: Float -> String
formatNumber n = printf "%.2f" n




--Gets as input a 'City' object, and and gives back a string version of the city name and the population
cityToString :: City -> String
cityToString city = cityName city ++ ": " ++ show (cityPopulation city)


--Takes input a 'City' object and gives back a string of the cities with population >1000000
citiesToString :: [City] -> String
citiesToString cities = unlines $ map cityToString $ filter (\city -> cityPopulation city > [1000000]) cities


--Takes a list of 'City' object and print city names on separated lines
printCityMap :: [City] -> IO ()
printCityMap cities =
  putStrLn $ unlines $ map (\city -> cityName city) cities




-------------------------------------------------------------------------------------------------------------------------------------------------------




-- Main function  - ACTUAL !

--{-

main :: IO ()
main = do
  putStrLn "Welcome to City Manager!"
  cities <- loadCitiesFromFile "cities.txt"
  showMenu cities

---}



-- Main function  - demos !

{-
main :: IO ()
main = do
  putStrLn "Welcome to City Manager!"
  cities <- loadCitiesFromFile "cities.txt"
  putStrLn "Enter demo number (1-8):"
  demoNumber <- readLn
  demo demoNumber
-}




-----------------------------------------------------------------------------------------------------------------------------------------



-- File I/O functions

--Takes a filepath as input and loads cities from file.
loadCitiesFromFile :: FilePath -> IO [City]
loadCitiesFromFile fileName = do
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      contents <- readFile fileName
      let cities = read contents :: [City]
      putStrLn "Cities loaded from file."
      return cities
    else do
      putStrLn "Cities file not found. Starting with an empty list."
      return []

--Takes a filepath and a list of Cities as input and writes a list of citires to file (specified), using the 'show' function
writeCitiesToFile :: FilePath -> [City] -> IO ()
writeCitiesToFile fileName cities = do
  writeFile fileName (show cities)
  putStrLn "Cities saved to file."



-------------------------------------------------------------------------------------------------------------------------------------------


-- Utility functions

--Finds City by name and returns it
findCityByName :: String -> [City] -> Maybe City
findCityByName name cities = find (\city -> cityName city == name) cities

--Shows A city
displayCity :: Maybe City -> IO ()
displayCity Nothing = putStrLn "City not found."
displayCity (Just city) = putStrLn (show city)

--Shows all cities
displayAllCities :: [City] -> IO ()
displayAllCities cities = do
  putStrLn "All Cities:"
  mapM_ (putStrLn . show) cities 




--Adds A city (asks for cityName, cityLongitute, cityLatitude, cityPopulation)
addCity :: [City] -> IO [City]
addCity cities = do
  putStrLn "Enter city name:"
  name <- getLine
  putStrLn "Enter city longitude:"
  longitude <- readFloat
  putStrLn "Enter city latitude:"
  latitude <- readFloat
  putStrLn "Enter city population (comma-separated list of floats):"  --Updated prompt for a list of floats
  populationStr <- getLine
  let population = read populationStr :: [Float]  --Parses the string input to a list of floats
  let city = City name longitude latitude population
  return (city : cities)


--Removes A city (asks for cityName)
removeCity :: [City] -> IO [City]
removeCity cities = do
  putStrLn "Enter city name to remove:"
  name <- getLine
  let updatedCities = filter (\city -> cityName city /= name) cities
  if length updatedCities == length cities
    then do
      putStrLn "City not found."
      return cities
    else do
      putStrLn "City removed."
      return updatedCities



--Plots city map
plotCityMap :: [City] -> IO ()
plotCityMap cities = putStrLn "Plotting city map..." --This is a placeholder for actual implementation


--Reads an integer input
readInt :: IO Int
readInt = do
  input <- getLine
  case readMaybe input of
    Just n -> return n
    Nothing -> putStrLn "Invalid input. Please enter an integer." >> readInt


--Reads a float input
readFloat :: IO Float
readFloat = do
  input <- getLine
  case readMaybe input of
    Just f -> return f
    Nothing -> putStrLn "Invalid input. Please enter a float." >> readFloat




------------------------------------------------------------------------------------------------------------------------------






--Show menu (shows a list of the options the user can choose from to interact with the program)
--After the user chooses an option, it calls the related function to execute what the uses wants to do
showMenu :: [City] -> IO ()
showMenu cities = do
  putStrLn "Menu:"
  putStrLn "1. Add city"
  putStrLn "2. Remove city"
  putStrLn "3. Find city by name"
  putStrLn "4. Display all cities"
  putStrLn "5. Plot city map"
  putStrLn "6. Save cities to file"
  putStrLn "7. Exit"
  putStrLn "Enter your choice:"
  choice <- getLine
  case choice of
    "1" -> do
      updatedCities <- addCity cities
      showMenu updatedCities
    "2" -> do
      updatedCities <- removeCity cities
      showMenu updatedCities
    "3" -> do
      putStrLn "Enter city name to find:"
      name <- getLine
      displayCity (findCityByName name cities)
      showMenu cities
    "4" -> do
      displayAllCities cities
      showMenu cities
    "5" -> do
      plotCityMap cities
      showMenu cities
    "6" -> do
      writeCitiesToFile "cities.txt" cities
      putStrLn "Cities saved to file."
      showMenu cities
    "7" -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid input. Please enter a valid choice."
      showMenu cities




