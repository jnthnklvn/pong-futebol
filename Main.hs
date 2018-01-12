module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import WorldPES

-- Janela principal com dimensoes e posicao inicial
window :: Display
window = InWindow "PES (Pong Evolution Soccer)" (600, 600) (0, 0)

-- Cor de fundo da janela
background :: Color
background = black

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
    -- Cria a bola com raio 15 e cor verde, posicionada em coordBola
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

-- Eventos de teclado
fctKeys :: Event -> WorldPES -> WorldPES
-- Quando a tecla 'r' é pressionada o jogo reinicia
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