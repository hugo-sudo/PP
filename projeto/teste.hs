readI :: String -> Int
readI = read


distancia :: [String] -> Int
distancia [] = 0
distancia (x:y:ys) = fromEnum(y) - fromEnum(x) + distancia ys