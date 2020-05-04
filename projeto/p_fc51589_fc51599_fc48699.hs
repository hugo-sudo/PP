import Hora
import System.IO
import Data.List
import Data.Char
import System.Environment


main = do
    args <- getArgs
    filePercurso <- readFile (args !! 0)
    fileInteresse <- readFile (args !! 1)
    let contentLines = getLatitude (lines (filePercurso))
    let hours = tempoTotal (getHora (lines (filePercurso)))
    let ganhototal = round (ganhoAcumuladoTotal (getElevacao (lines (filePercurso))))
    let ganhoMetro = ganhoAcumuladoPorMetro  (getLatitude ( lines (filePercurso))) (getLongitude (lines (filePercurso))) (getElevacao (lines (filePercurso)))
    let categoriaPercurso = categoria (getLatitude ( lines (filePercurso))) (getLongitude (lines (filePercurso))) (getElevacao (lines (filePercurso)))
    let paragens = getParagens (lines (fileInteresse))
    writeFile (args !! 2) $ finalJson (categoriaPercurso) (hours) (ganhototal) (ganhoMetro) (paragens)
    

getLatitude :: [String] -> [String]
getLatitude [] = []
getLatitude (x:xs) = [head(split (x))] ++ getLatitude xs

getLongitude :: [String] -> [String]
getLongitude [] = []
getLongitude (x:xs) = [(split (x) !! 1)] ++ getLongitude xs

getElevacao :: [String] -> [String]
getElevacao [] = []
getElevacao (x:xs) = [(split (x) !! 2)] ++ getElevacao xs

getParagens :: [String] -> [String]
getParagens [] = []
getParagens (x:xs) = [(split (x) !! 2)] ++ getParagens xs

getHora :: [String] -> [String]
getHora [] = []
getHora (x:xs) = [(split (x) !! 3)] ++ getHora xs

tempoTotal :: [String] -> Int
tempoTotal xs = convertMinutes (calculateHours (time (head xs)) (time (last (xs))))

ganhoAcumuladoPorMetro :: [String] -> [String] -> [String] -> Float
ganhoAcumuladoPorMetro [] [] [] = 0
ganhoAcumuladoPorMetro (xs) (ys) (zs) =  fromInteger ((round ((ganhoAcumuladoTotal zs / (ganhoAcumuladoPorMetroA (xs) (ys) )) * (10 **3)))) / 10 ** 3

ganhoAcumuladoPorMetroA :: [String] -> [String] -> Float
ganhoAcumuladoPorMetroA [] [] = 0
ganhoAcumuladoPorMetroA (x:[]) (y:[]) = 0
ganhoAcumuladoPorMetroA (x1:x2:xs) (y1:y2:ys) =  sum (distanciaPontos (read(x1) *85000 ,read(y1) *85000) (read(x2) *85000 ,read(y2) *85000) : ganhoAcumuladoPorMetroA (x2:xs) (y2:ys) :[])

distanciaPontos :: (Float, Float)-> (Float, Float) -> Float
distanciaPontos (x1,y1) (x2,y2) = sqrt( (x2 - x1) ^2 + (y2 - y1) ^2)

categoria :: [String] -> [String] -> [String] -> [Char]
categoria [] [] [] = []
categoria xs ys zs 
  | ((ganhoAcumuladoTotal (zs)) / 1000) < 500 && ((ganhoAcumuladoPorMetroA xs ys) / 1000) < 10 = "A"
  | ((ganhoAcumuladoTotal (zs)) / 1000) < 800 && ((ganhoAcumuladoPorMetroA xs ys) / 1000) < 12= "B"
  | ((ganhoAcumuladoTotal (zs)) / 1000) < 1500 && ((ganhoAcumuladoPorMetroA xs ys) / 1000 )< 15 = "C"
  | otherwise = "D"

ganhoAcumuladoTotal :: [String] -> Float
ganhoAcumuladoTotal [] = 0
ganhoAcumuladoTotal (x:[]) = 0
ganhoAcumuladoTotal (x:y:ys)
 | (read y :: Float) > (read x :: Float)= read(y) - read(x) + ganhoAcumuladoTotal (y:ys)
 | otherwise = 0 + ganhoAcumuladoTotal (y:ys)


split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == ','  = "" : rest
   | otherwise = (c : head rest) : tail rest
 where rest = split cs


finalJson :: String -> Int -> Int -> Float -> [String] -> String
finalJson a b c d xs = "Categoria : " ++ show (a) ++ "\n" ++ "Tempo total (m) : " ++ show (b) ++ "\n" ++ "Ganho acumulado : " ++ show (c) ++ "\n" ++ "Ganho acumulado por m : " ++ show (d) ++ "\n" ++ "Pontos de Interesse : " ++ show (xs) ++ "\n"