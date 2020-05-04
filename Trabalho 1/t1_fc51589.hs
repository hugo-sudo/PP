triangulacao :: ((Float , Float),(Float , Float ),(Float , Float )) -> (Float , Float )
triangulacao ((x1, y1),(x2, y2),(x3, y3)) = ((x1 + x2 + x3)/3.0, (y1 + y2 + y3)/3.0)

distanciaOrigem :: [(Float , Float)] -> [Float]
distanciaOrigem x = [sqrt(a**2.0 + b**2.0) | (a,b) <- x]

proximoPonto :: [(Float , Float)] -> (Float , Float)
proximoPonto lst = (c - a + c , d - b + d)
    where a = fst (lst !! (length lst - 2))
          b = snd (lst !! (length lst - 2))
          c = fst (lst !! (length lst - 1))
          d = snd (lst !! (length lst - 1))