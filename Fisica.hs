module Fisica where

-- Definicao de tipos para auxiliar as chamadas
type Radius = Float
type Position = (Float, Float)

-- Checa se a bola toca nas paredes verticalmente
pVCollision :: Position -> Bool
pVCollision (_, y) = nCollision || sCollision
  where
    sCollision = y-15<=(-297.5)
    nCollision = y+15>=297.5

-- Checa se a bola toca nas paredes horizontalmente
pHCollision :: Position -> Bool
pHCollision (x, y) = (eCollision || wCollision) && dc
  where
    dc = ((y-15)<=(-90)) || ((y+15)>=90)
    eCollision = (x-15)<=(-297.5)
    wCollision = (x+15)>=297.5

-- Calcula a distancia entre dois pontos e a retorna como Float
d2p :: Position -> Position -> Float
d2p (x,y) (x1,y1) = sqrt (((x-x1)**2) + ((y-y1)**2))

-- Checa se a bola toca no bastao oeste, recebendo a posicao da bola, raio e posicao do bastao
bwCollision :: Position -> Position -> Bool
bwCollision (x, y) (x1, y1) = wCollision
  where
    wCollision = (y-15<=y1+30) && (y+15>=y1-30) && (x-15<= -285) && (x-15> -295)

-- Checa se a bola toca no bastao leste, recebendo a posicao da bola, raio e posicao do bastao
beCollision :: Position -> Position -> Bool
beCollision (x, y) (x1, y1) = eCollision
  where
    eCollision = (y-15<=y1+30) && (y+15>=y1-30) && (x+15>=285) && (x+15<295)

-- Checa se a bola toca nas quinas dos bastoes ou das paredes
quinaCollision :: Position -> (Position,Position) -> Bool
quinaCollision (x, y) ((bx1,by1),(bx2,by2)) = qp1Collision || qp2Collision || qb1Collision || qb2Collision
  where
    qp1Collision = (((d2p (x,y) (297.5,90))<=15) && (y>95)) || (((d2p (x,y) (297.5,-90))<=15) && (y<(-95)))
    qp2Collision = (((d2p (x,y) (-297.5,90))<=15) && (y>95)) || (((d2p (x,y) (-297.5,-90))<=15) && (y<(-95)))
    qb1Collision = (y>=by1+35 && y-15<=by1+35 || (y>=by1-35 && y+15<=by1-35)) && x+15>= 285
    qb2Collision = (y>=by2+35 && y-15<=by2+35 || (y>=by2-35 && y+15<=by2-35)) && x-15<= -285
