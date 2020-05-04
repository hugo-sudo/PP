distancia :: [(Float, Float)] -> Float
distancia a = foldl1 (+) (distanciaAux a)

distanciaAux :: [(Float, Float)] -> [Float]
distanciaAux [] = [0]
distanciaAux [x] = [0] 
distanciaAux (x:xs) = ([sqrt((fst(head(xs)) - fst(x))**2.0 + (snd(head(xs)) - snd(x))**2.0)] ++ distanciaAux xs)

minimaDistanciaAAux :: (Float, Float) -> (Float, Float) -> Float
minimaDistanciaAAux a b = sqrt(((fst(b) - fst(a))**2.0 + (snd(b) - snd(a))**2.0))

minimaDistanciaA :: (Float, Float) -> [(Float, Float)] -> Float
minimaDistanciaA a [] = 0
minimaDistanciaA a [x] = minimaDistanciaAAux a x
minimaDistanciaA a xs = minimum( map (minimaDistanciaAAux a) (xs))

evitaPontosAux :: Float -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)]
evitaPontosAux d [x] ys = filter (\y -> minimaDistanciaAAux x y > d) (ys)
evitaPontosAux d (x:xs) ys = filter (\y -> minimaDistanciaAAux x y > d) (ys) ++ evitaPontosAux d xs ys

evitaPontosAux' :: [(Float,Float)] -> [(Float,Float)]
evitaPontosAux' [] = []
evitaPontosAux' [x] = []
evitaPontosAux' (x:xs) 
    | length((filter (\z -> x == z) xs)) >=1 = x:evitaPontosAux' xs
    | length((filter (\z -> x == z) xs)) == 0 = evitaPontosAux' xs

evitaPontos :: Float -> [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)]
evitaPontos d (x:xs) ys = evitaPontosAux' (evitaPontosAux d (x:xs) ys)

