module WorldPES where

import Fisica

-- Data do mundo do game
data WorldPES = GameOver String | Game{
 coordBola :: (Float, Float),  -- Coordenadas da bola (x, y)
 velBola :: (Float, Float),  -- Velocidade da bola nos eixos (x, y)
 bastao1 :: (Float, Float, Float),  -- Bastao 1 coordenadas (x, y)
 bastao2 :: (Float, Float, Float)  -- Bastao 2 coordenadas (x, y)
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

    -- Limita o movimento do bastao do jogador
    d2 = if (by-35<=(-300)) then 2
        else if (by+35>=300) then -2
        else d0
    (cx,cy,_) = (bx,by+d0,d2)

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