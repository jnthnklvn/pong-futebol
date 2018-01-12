module Fisica where

-- Definicao de tipos para auxiliar as chamadas
type Position = (Float, Float)
raio = 15

-- Checa se a bola colide nas paredes verticalmente
pVCollision :: Position -> Bool
pVCollision (_, y) = nCollision || sCollision
  where
    sCollision = y-raio<=(-297.5)
    nCollision = y+raio>=297.5

-- Checa se a bola colide nas paredes horizontalmente
pHCollision :: Position -> Bool
pHCollision (x, y) = (eCollision || wCollision) && dc
  where
    dc = ((y-raio)<=(-90)) || ((y+raio)>=90)
    eCollision = (x-raio)<=(-297.5)
    wCollision = (x+raio)>=297.5

-- Checa se a bola colide com um dos bastoes
bCollision :: Position -> Position -> Bool
bCollision (x,y) (x1,y1) = not (xCheck x1 x) && not (yCheck y1 y)
  where
    xCheck x1 x = x1+raio < x-5 || x1-raio > x+5
    yCheck y1 y = y1-raio > y+35 || y1+raio < y-35