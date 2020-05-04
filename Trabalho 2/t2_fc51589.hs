distancia :: [(Float, Float)] -> Float
distancia [x] = 0 
distancia (x:xs) = sqrt((fst(head(xs)) - fst(x))**2.0 + (snd(head(xs)) - snd(x))**2.0) + distancia xs

fundePercursos :: [(Float, Float)] -> [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
fundePercursos [] [] _ = []
fundePercursos [x] [] _ = x:fundePercursos [] [] x
fundePercursos [] [y] _ = y:fundePercursos [] [] y
fundePercursos (x:xs) (y:ys) z
    | distancia [x,z] <= distancia [y,z] = z:fundePercursos xs (y:ys) x
    | distancia [x,z] > distancia [y,z] = z:fundePercursos (x:xs) ys y

adicionaParagem :: [(Float,Float)] -> (Float,Float) -> [(Float,Float)]
adicionaParagem xs y = adicionaParagemAux' xs y 1

adicionaParagemAux :: [(Float,Float)] -> (Float,Float) -> Int -> [Float]
adicionaParagemAux xs y n
    | n < length xs = [distancia (take n xs ++ [y] ++ drop n xs)] ++ adicionaParagemAux xs y (n+1)
    | n >= length xs = []

adicionaParagemAux' :: [(Float,Float)] -> (Float,Float) -> Int -> [(Float,Float)]
adicionaParagemAux' xs y n
    | minimum (adicionaParagemAux xs y 1) == distancia (take n xs ++ [y] ++ drop n xs) = take n xs ++ [y] ++ drop n xs
    | (n < length xs) && (minimum (adicionaParagemAux xs y 1) /= distancia (take n xs ++ [y] ++ drop n xs)) = adicionaParagemAux' xs y (n+1)
    | n >= length xs = []