module Rotas 
( Rota
, Paragem
, criaRota
, criaRotaAux
, adicionaTecnica
, adicionaTecnicaAux
, pontoParagem
, percursoRota
, verificaParagem
, showAux
) where

import Geometria

 
data Paragem = PontoInteresse String Ponto | ParagemTecnica Ponto
data Rota = Rota String [Paragem]

pontoParagem :: Paragem -> Ponto
pontoParagem (PontoInteresse _ p) = p
pontoParagem (ParagemTecnica p) = p

percursoRota :: [Paragem] -> Percurso
percursoRota [] = []
percursoRota (x:xs) = [pontoParagem x] ++ percursoRota xs

verificaParagem :: Paragem -> String
verificaParagem (PontoInteresse a _) = a
verificaParagem (ParagemTecnica _) = "(Pausa)"

criaRota :: String -> [String] -> Percurso -> Rota
criaRota a [x] [y] = Rota a ([PontoInteresse x y])
criaRota a xs ys = Rota (a) (criaRotaAux xs ys)

criaRotaAux :: [String] -> Percurso -> [Paragem]
criaRotaAux [] ys = []
criaRotaAux xs [] = []
criaRotaAux [x] [y] = [PontoInteresse x y]
criaRotaAux (x:xs) (y:ys) 
    | length xs >= 0 || length ys >= 0 = (PontoInteresse x y):(criaRotaAux xs ys)
    | otherwise = []
  
adicionaTecnica :: Int -> Ponto -> Rota -> Rota
adicionaTecnica a b (Rota s ps) = Rota s (adicionaTecnicaAux ps a b)

adicionaTecnicaAux :: [Paragem] -> Int -> Ponto -> [Paragem]
adicionaTecnicaAux xs a b = (take (a) xs) ++ [ParagemTecnica b] ++ (drop (length (xs) - 1 - a) xs)

showAux :: [Paragem] -> String 
showAux [] = []
showAux [x] = verificaParagem (x)
showAux (x:xs)
    | verificaParagem (x) /= "(Pausa)" = verificaParagem (x) ++ " --- " ++ showAux (xs)
    | otherwise = "(Pausa)" ++ " --- " ++ showAux (xs)


instance Show Rota where
  show (Rota a xs) = a ++ " (" ++ show (round(distanciaPercurso(percursoRota(xs)))) ++ "): " ++ showAux (xs)

  



  