ganhoAcumuladoPorMetro :: [String] -> [String] -> [String] -> Float
ganhoAcumuladoPorMetro [] [] [] = 0
ganhoAcumuladoPorMetro (xs) (ys) (zs) = (ganhoAcumuladoPorMetroA (xs) (ys) ) / ganhoAcumuladoTotal (zs)

ganhoAcumuladoPorMetroA :: [String] -> [String] -> Float
ganhoAcumuladoPorMetroA [] [] = 0
ganhoAcumuladoPorMetroA (x:[]) (y:[]) = 0
ganhoAcumuladoPorMetroA (x1:x2:xs) (y1:y2:ys) =  sum (distanciaPontos (read(x1) *85 ,read(y1) *85) (read(x2) *85 ,read(y2) *85) : ganhoAcumuladoPorMetroA (x2:xs) (y2:ys) :[])

distanciaPontos :: (Float, Float)-> (Float, Float) -> Float
distanciaPontos (x1,y1) (x2,y2) = sqrt( (x1 - x2) ^2 + (y1 - y2) ^2)
