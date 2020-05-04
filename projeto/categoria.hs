categoria :: [String] -> [String] -> [String] -> [Char]
categoria [] [] [] = []
categoria xs ys zs 
  |(ganhoAcumuladoTotal (zs)) < 500 && (ganhoAcumuladoPorMetroA xs ys) < 10 = "A"
  |(ganhoAcumuladoTotal (zs)) < 800 && (ganhoAcumuladoPorMetroA xs ys) < 12 = "B"
  |(ganhoAcumuladoTotal (zs)) < 1500 && (ganhoAcumuladoPorMetroA xs ys) < 15 = "C"
  |otherwise = "D"
