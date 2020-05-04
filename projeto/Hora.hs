module Hora
( timeToS
, timeToI
, time
, splitHour
, getHours
, getMinutes
, getSeconds
, calculateHours
, convertMinutes
) where

timeToS :: String -> [String]
timeToS xs = splitHour xs

timeToI :: [String] -> [Int]
timeToI (x:y:z:xs) = [read (x), read (y), read (z)]

time :: String -> [Int]
time hour = timeToI (timeToS hour)

getHours :: [Int] -> Int
getHours (x:y:z:xs) = x

getMinutes :: [Int] -> Int
getMinutes (x:y:z:xs) = y

getSeconds :: [Int] -> Int
getSeconds (x:y:z:xs) = z

calculateHours :: [Int] -> [Int] -> [Int]
calculateHours [] [] = [0]
calculateHours xs [] = [0]
calculateHours [] ys = [0]
calculateHours xs ys
   | ((getSeconds ys) - (getSeconds xs)) < 0 && ((getMinutes ys) - (getMinutes xs)) > 0 = [(getHours ys) - (getHours xs), ((getMinutes ys) - (getMinutes xs)) - 1 , 60 + ((getSeconds ys) - (getSeconds xs))]
   | ((getSeconds ys) - (getSeconds xs)) < 0  && ((getMinutes ys) - (getMinutes xs)) < 0 = [((getHours ys) - (getHours xs)) - 1, 60 + (((getMinutes ys) - (getMinutes xs)) - 1), 60 + ((getSeconds ys) - (getSeconds xs))]
   | ((getSeconds ys) - (getSeconds xs)) > 0  && ((getMinutes ys) - (getMinutes xs)) < 0 = [((getHours ys) - (getHours xs)) - 1, 60 + ((getMinutes ys) - (getMinutes xs)), ((getSeconds ys) - (getSeconds xs))]
   | ((getSeconds ys) - (getSeconds xs)) > 0  && ((getMinutes ys) - (getMinutes xs)) > 0 = [(getHours ys) - (getHours xs), (getMinutes ys) - (getMinutes xs) , (getSeconds ys) - (getSeconds xs)]

convertMinutes :: [Int] -> Int
convertMinutes (x:y:z:xs) = (x * 60) + y + (z`quot`60)

splitHour :: String -> [String]
splitHour [] = [""]
splitHour (c:cs)
   | c == ':'  = "" : rest
   | otherwise = (c : head rest) : tail rest
 where rest = splitHour cs