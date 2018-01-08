module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Janela principal com dimensoes e posicao inicial
window :: Display
window = InWindow "PES (Pong Evolution Soccer)" (600, 600) (0, 0)

-- Cor de fundo da janela
background :: Color
background = black

-- Definicao de tipos para auxiliar as chamadas
type Radius = Float
type Position = (Float, Float)

-- Data do mundo do game
data WorldPES = GameOver String | Game{
 coordBola :: Position,  -- Coordenadas da bola (x, y)
 velBola :: Position,  -- Velocidade da bola nos eixos (x, y)
 bastao1 :: (Float, Float, Float),  -- Bastao 1 coordenadas (x, y)
 bastao2 :: (Float, Float, Float)  -- Bastao 2 coordenadas (x, y)
}deriving Show

-- O estado inicial do game
eInicial :: WorldPES
eInicial = Game{
 coordBola = (0, 0), -- Coordenadas da bola
 velBola = (-300, 110), -- Velocidade da bola nos eixos (x,y)
 bastao1 = (290, 0, 1.5), -- Jogador artificial
 bastao2 = (-290, 0, 0) -- Jogador
}

-- Renderiza o game em uma Picture
render :: WorldPES -> Picture
render (GameOver s) = scale 0.5 0.5 . translate (-300) 0
     . color green . text $ s
render game = pictures[bola, paredesH, paredesV,
                       cBastao (bastao1 game),
                       cBastao (bastao2 game)]
 where
    -- Cria a bola com raio 15 e cor preta, posicionada em coordBola
    bola = uncurry translate (coordBola game) (color green (circleSolid 15))

    -- Criadores de paredes verticais e horizontais, respectivamente
    paredeH :: Float -> Picture
    paredeH y = translate 0 y (color green (rectangleSolid 600 5))
    paredeV :: Float -> Float -> Picture
    paredeV x y = translate x y (color green (rectangleSolid 5 210))

    -- Criacao de Pictures das paredes verticais e horizontais
    paredesH = pictures [paredeH 297.5, paredeH (-297.5)]
    paredesV = pictures [paredeV 297.5 195, paredeV 297.5 (-195),
                         paredeV (-297.5) (-195), paredeV (-297.5) (195)]

    --  Criador de bastoes
    cBastao :: (Float, Float, Float) -> Picture
    cBastao (x, y, _) = pictures
      [ translate x y (color green (rectangleSolid 10 70)),
        translate x y (color green (rectangleSolid 10 70))]

-- Movimenta o bastao, sendo chamada enquanto as teclas chaves estao pressionadas
attBastao :: Float -> WorldPES -> WorldPES
attBastao vel game = game {bastao2 = (dx,dy,d)}
    where
      -- Coordenadas atuais
      (bx,by,d0) = bastao2 game
      -- Novas coordenadas do bastao2
      (dx,dy,d) = if (d0==vel) then (bx,by,0)
        else (bx,by,vel)

-- Atualiza as coordenadas da bola e do bastao1 (jogador artificial) e limita os movimentos do bastao2 
attBolaeIA :: Float -> WorldPES -> WorldPES
attBolaeIA _ (GameOver s) = GameOver s
attBolaeIA time game = game {
 coordBola = (x1, y1),
 bastao1 = (dx,dy,d1),
 bastao2 = (cx,cy,d2)
}
  where
    -- Atuais coordenadas e velocidade da bola.
    (x, y) = coordBola game
    (vx, vy) = velBola game
    -- Coordenadas e velocidade atuais do bastao 1 e 2
    (ax,ay,d) = bastao1 game
    (bx,by,d0) = bastao2 game
    -- Novas coordenadas da bola
    x1 = x + vx * time
    y1 = y + vy * time

    -- Limita e automatiza a movimentacao do jogador artificial
    d1 = if (ay<(-80)) then 1.5
         else if (ay>80) then -1.5
         else d
    (dx,dy,_) = (ax,ay+d,d1)

    -- Limita o movimento do bastao do jogador
    d2 = if (by-35<=(-300)) then 2
        else if (by+35>=300) then -2
        else d0
    (cx,cy,_) = (bx,by+d0,d2)

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

-- Checa se a bola toca no bastao leste, recebendo a posicao da bola, raio e posicao do bastao
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

-- Checa as colisoes da bola e retorna o mundo com as atualizacoes de velocidade
vrfCollision :: WorldPES -> WorldPES
vrfCollision (GameOver s) = GameOver s
vrfCollision game = q
  where
    -- As velocidades atuais da bola nos eixos (x, y)
    (vx, vy) = velBola game
    -- Coordenadas dos bastoes
    (x1,y1,d1) = bastao1 game
    (x2,y2,d2) = bastao2 game
    (x,y) = coordBola game

    q = if (x>=315) then GameOver "You won!"
        else if (x<= -315) then GameOver "Se fodeu!"
        else  game {velBola = (vx1, vy1)}

    -- Verifica colisao verticalmente e altera a velocidade e o sentido, caso haja
    vy1 = if (pVCollision (coordBola game) || quinaCollision (coordBola game) ((x1,y1),(x2,y2)))
          then -vy*1.05
          -- Atualiza a velocidade no eixo x, somando a velocidade do bastao leste a bola
          else if (beCollision (coordBola game) (x1,y1)) then vy+(d1*10)
          -- Atualiza a velocidade no eixo x, somando a velocidade do bastao oeste a bola 
          else if (bwCollision (coordBola game) (x2,y2)) then vy+(d2*10)
          else vy

    -- Verifica colisao horizontalmente e altera o sentido, caso haja
    vx1 = if (pHCollision (coordBola game)) then -vx
          else if ((beCollision (coordBola game) (x1,y1))) || ((bwCollision (coordBola game) (x2,y2)))
          then -vx
          else vx

-- Eventos de teclado
fctKeys :: Event -> WorldPES -> WorldPES
-- Quando a tecla 'r' é pressionada a bola volta ao centro (0, 0) coma velocidade inicial (-300, 150)
fctKeys (EventKey (Char 'r') _ _ _) game = eInicial
-- Quando as teclas cima ou baixo são pressionadas o bastao se move a 1.3 pixels em y
fctKeys (EventKey (SpecialKey KeyUp) _ _ _) (GameOver s) = GameOver s
fctKeys (EventKey (SpecialKey KeyDown) _ _ _) (GameOver s) = GameOver s
fctKeys (EventKey (SpecialKey KeyUp) _ _ _) game = attBastao 1.3 game
fctKeys (EventKey (SpecialKey KeyDown) _ _ _) game = attBastao (-1.3) game
fctKeys _ game = game

-- Atualiza o mundo do game com a frequencia s (segundos)
update :: Float -> WorldPES -> WorldPES
update s = attBolaeIA s . vrfCollision

-- Frames per second
fps :: Int
fps = 160

main :: IO ()
main = play window background fps eInicial render fctKeys update