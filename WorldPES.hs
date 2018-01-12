module WorldPES where

import Fisica

-- Data do mundo do game
data WorldPES = GameOver String | Game{
 coordBola :: (Float,Float),  -- Coordenadas da bola (x, y)
 velBola :: (Float,Float),  -- Velocidade da bola nos eixos (x, y)
 bastao1 :: (Float, Float, Float),  -- Bastao 1 coordenadas e velocidade vertical (x, y, d)
 bastao2 :: (Float, Float, Float)  -- Bastao 2 coordenadas e velocidade vertical (x, y, d)
}deriving Show

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

    -- Limita o movimento do bastao do jogador aos limites verticais do campo
    d2 = if (by-35<=(-300)) then 2
        else if (by+35>=300) then -2
        else d0
    (cx,cy,_) = (bx,by+d0,d2)

-- Atualiza sentido, coordenadas e velocidade da bola
bHCollision :: WorldPES -> Float -> WorldPES
bHCollision game x' = game{velBola = (-x1,y1+(d*10)), coordBola=(x',y)}
  where
    (x,y) = coordBola game
    (x1,y1) = velBola game
    (_,_,d) = if (x<0) then bastao2 game else bastao1 game

-- Atualiza sentido, coordenadas e velocidade da bola
bVCollision :: WorldPES -> Float -> WorldPES
bVCollision game y' = game{velBola = (x1,-(y1+(d*10))), coordBola=(x,y')}
  where
    (x,y) = coordBola game
    (x1,y1) = velBola game
    (_,_,d) = if (x<0) then bastao2 game else bastao1 game

-- Identifica onde a bola colide com o bastao
bACollision :: WorldPES -> Position -> WorldPES
bACollision game (x,y)
  -- Verifica se a bola colide em x e y
  | min yt yb == min xl xr = dCollision

  -- Verifica se a bola colide na parte de cima do bastao
  | yt < yb = if xr < xl then tR else tL

  -- A bola colide na parte de baixo do bastao
  | otherwise = if xr < xl then bR else bL
  where
    (x1,y1) = coordBola game
    yt = (y+35)-(y1-raio)
    yb = (y1+raio)-(y-35)
    xl = (x1+raio)-(x-5)
    xr = (x+5)-(x1-raio)
    tL = if yt < xl then bVCollision game (y+35+raio)
                  else bHCollision game (x-5-raio)

    tR = if yt < xr then bVCollision game (y+35+raio)
                  else bHCollision game (x+5+raio)

    bL = if yb < xl then bVCollision game (y-35-raio)
                  else bHCollision game (x-5-raio)

    bR = if yb < xr then bVCollision game (y-35-raio)
                  else bHCollision game (x+5+raio)

    dCollision = let yPos = if yt < yb then y+35+raio else y-35-raio
                      xPos = if xr < xl then x+5+raio else x-5-raio
                  in bVCollision (bHCollision game xPos) yPos

-- Checa as colisoes da bola e retorna o mundo com as atualizacoes de velocidade
vrfCollision :: WorldPES -> WorldPES
vrfCollision (GameOver s) = GameOver s
vrfCollision game = q
  where
    -- Coordenadas dos bastoes e velocidades da bola
    (x1,y1,d1) = bastao1 game
    (x2,y2,d2) = bastao2 game
    (x,y) = coordBola game
    (vx, vy) = velBola game

    -- Retorna uma tela de GameOver se a bola estiver com o centro a 15 pixels fora do campo
    q = if (x>=286) then GameOver "You won!" --Jogador ganhou
        else if (x<= -286) then GameOver "Se fodeu!" --Jogador artificial ganhou
        else if (bCollision (x1,y1) (x,y)) then (bACollision game (x1,y1)) --Colisoes jogador artificial
        else if (bCollision (x2,y2) (x,y)) then (bACollision game (x2,y2)) --Colisoes jogador
        else  game {velBola = (vx1, vy1)} --Colisoes paredes

    -- Verifica colisao verticalmente e altera a velocidade e o sentido, caso haja
    vy1 = if pVCollision (coordBola game) then -vy*1.05 -- Muda o sentido da velocidade e a aumenta em 5%
          else vy -- Retorna a mesma velocidade

    -- Verifica colisao horizontalmente e altera o sentido, caso haja
    vx1 = if pHCollision (coordBola game) then -vx -- Muda o sentido da velocidade
          else vx -- Retorna a mesma velocidade