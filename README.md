# Pong-Futebol
  * Jogo de futebol estilo ping pong usando módulo Gloss do *Haskell*.
  <p align="center">
  <img src="https://user-images.githubusercontent.com/25539501/34655018-e2e4bc18-f3ea-11e7-8287-4447e641ee85.png">
  </p>

## 1. Funcionamento
  * O jogo é disputado entre dois oponentes: um jogador humano e um jogador artificial.
Os jogadores disputam pela realização de um gol.

  * O jogador pode movimentar um bastão no sentido vertical para defender seu gol ou direcionar o movimento da bola,
enquanto se defende do jogador artificial.

## 2. Movimentação
* Movimento da bola
  * Velocidade horizontal: constante e muda o sentido sempre que colide horizontalmente.
  * Velocidade vertical: muda sempre que colide com os bastões. A velocidade dos bastões é somada a da bola ou
  quando colide verticalmente.

* Movimento dos bastões
  * Os bastões se movimentam no sentido vertical. O bastão do jogador se move quando as teclas Up e Down são
pressionadas, e continua se movendo enquanto as teclas estiverem pressionadas, e sobe e desce automaticamente quando
antige os limites sul e norte do campo, respectivamente. O bastão artificial tem  um movimento limitado as proximidades
do gol, subindo e descendo com velocidade padrão.

## 3. Dimensões
  * O campo tem dimensões 600x600 pixels.
  * As traves possuem altura igual a 180 pixels centralizadas verticalmente em cada extremidade horizontal do campo.
  * A bola possui raio igual 15 pixels.
  * Os bastões possuem dimensões 70 pixels na vertical e 10 pixels na horizontal.

## 4. Tela final
  * O jogo acaba quando a bola passar completamente pelo gol. A tela final mostra um Text indicando o vencedor.
  * Após o fim do jogo, é possivel o reinicio da partida com a telca "r".

  <p align="center">
  <img src="https://user-images.githubusercontent.com/25539501/34655225-5401a116-f3ed-11e7-8d81-0ad2d5060bfd.png">
  </p>
