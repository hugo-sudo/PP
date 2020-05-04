ganhoAcumuladoTotal :: [String] -> Float
ganhoAcumuladoTotal [] = 0
ganhoAcumuladoTotal (x:[]) = 0
ganhoAcumuladoTotal (x:y:xs)
 | y > x = read(y) - read(x) +  ganhoS
 | otherwise = 0 + ganhoS
 where ganhoS = ganhoAcumuladoTotal (y:xs)
