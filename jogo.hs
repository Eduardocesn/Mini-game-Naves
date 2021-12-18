{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Poligono = [Point]
type Direction = Double 
type Segmento = (Point,Point)

main = activityOf worldInitial
                  update 
                  visualize
        
--Tipo algébrico definindo a nave
data SpaceShip = 
  SpaceShip { posShip        :: Point,     --Posição em x e y
              velShip        :: Vector,    --Velocidade em x e y
              dirShip        :: Direction, --Direção da ponta da nave em radianos
              rotShip        :: Rotation,  --Estado da rotação (horário, antihorário ou sem giro)
              isAccele       :: Bool,      --True se estiver acelerando, False se não estiver
              isShooting     :: Bool,      --True se estiver atirando, False se não estiver
              hpShip         :: Int,       --Quantidade de tiros que a nave consegue resistir
              cooldownBullet :: Double,    --Controle do tempo de espera entre os tiros
              explosionTime  :: Double,    --Controle do tempo da explosão
              timeOutWorld   :: Double     --Controle do  tempo fora do "espaço visível"
              } deriving Show

--Tipo do giro da nave, horário, parado ou antihorário
data Rotation = ClockWise | Stop | CounterClock 
                deriving Show

--Tipo do projétil, um ponto de posição, um vetor de velocidade e um tempo de vida que determina o tempo de visualização desse tiro 
data Bullet = Bullet { posBullet :: Point,
                       velBullet :: Vector,
                       lifeTime  :: Double
                     } deriving Show

--O asteroide é um polígono com um timer para realizar a rotação
data Asteroid = Asteroid { pointsAste :: Poligono,
                           rotTimer :: Double
                         } deriving Show
                         
--O mundo é composto pelas 2 naves, os projéteis de ambas as naves, 2 asteroides e um timer de visualização dos controles
data BattleWorld =
      BattleSpace { player1, player2         :: SpaceShip, 
                    projeteisP1, projeteisP2 :: [Bullet], 
                    asteroid1, asteroid2     :: Asteroid,
                    controlsTimer            :: Double
                  } deriving Show

--Estado inicial da nave
shipInitial =
  SpaceShip { posShip        = (0,0),
              velShip        = (0,0),   --Velocidade zerada
              dirShip        = pi/2,    --Direção em pi/2 radianos (apontada para cima)
              rotShip        = Stop,    --Estado Stop (sem giro)
              isAccele       = False,   --Não está acelerando
              isShooting     = False,   --Não está atirando
              hpShip         = 10,      --Resistência inicial em 10 (a cada colisão com um projétil diminui em 1)
              cooldownBullet = 0,       --Tempo de recarga zerado (apto a atirar)
              explosionTime  = 0,       --Tempo de explosão em zero (pois a nave não está explodindo)
              timeOutWorld   = 0        --Tempo fora do espaço visível em 0 (pois a nave se encontra dentro do espaço)
            }

--Estado inicial da nave 1
player1Initial = shipInitial { posShip = (-5,-5)}
  
--Estado inicial da nave 2
player2Initial = shipInitial { posShip = (5,5)}
 
--Estado do mundo inicial
worldInitial =
    BattleSpace { player1 = player1Initial, 
                  player2 = player2Initial,
                  projeteisP1 = [],                           --Projéteis iniciais (lista vazia pois não possui nenhum projétil)
                  projeteisP2 = [],
                  asteroid1 = Asteroid (createAsteroid 7) 0,  --Construção dos asteroides (polígonos regulares)
                  asteroid2 = Asteroid (createAsteroid 6) 0,
                  controlsTimer = 0
                }
  
             
         

--Função de update
update :: Event -> BattleWorld -> BattleWorld
--Pressionar o botão W muda o estado de aceleração da nave 1 para True
update (KeyPress "W") battleSpace   = battleSpace { player1 = (player1 battleSpace) {isAccele = True} }
update (KeyRelease "W") battleSpace = battleSpace { player1 = (player1 battleSpace) {isAccele = False } }
--Pressionar o botão A muda o estado de rotação da nave 1 para Anti Horário
update (KeyPress "A") battleSpace   = battleSpace { player1 = (player1 battleSpace) { rotShip = CounterClock } }
update (KeyRelease "A") battleSpace = battleSpace { player1 = (player1 battleSpace) { rotShip = Stop } }
--Pressionar o botão D muda o estado de rotação da nave 1 para Horário
update (KeyPress "D") battleSpace   = battleSpace { player1 = (player1 battleSpace) { rotShip = ClockWise } }
update (KeyRelease "D") battleSpace = battleSpace { player1 = (player1 battleSpace) { rotShip = Stop } }
--Pressionar o botão E muda o estado de atirando da nave 1 para True
update (KeyPress "E") battleSpace   = battleSpace { player1 = (player1 battleSpace) { isShooting = True }}
update (KeyRelease "E") battleSpace = battleSpace { player1 = (player1 battleSpace) { isShooting = False }}

--Pressionar o botão I muda o estado de aceleração da nave 2 para True
update (KeyPress "I") battleSpace   = battleSpace { player2 = (player2 battleSpace) { isAccele= True } }
update (KeyRelease "I") battleSpace = battleSpace { player2 = (player2 battleSpace) { isAccele= False } } 
--Pressionar o botão J muda o estado de rotação da nave 2 para Anti Horário
update (KeyPress "J") battleSpace   = battleSpace { player2 = (player2 battleSpace) { rotShip = CounterClock } }
update (KeyRelease "J") battleSpace = battleSpace { player2 = (player2 battleSpace) { rotShip = Stop } }
--Pressionar o botão L muda o estado de rotação da nave 2 para Horário
update (KeyPress "L") battleSpace   = battleSpace { player2 = (player2 battleSpace) { rotShip = ClockWise } }
update (KeyRelease "L") battleSpace = battleSpace { player2 = (player2 battleSpace) { rotShip = Stop } }
--Pressionar o botão O muda o estado de atirando da nave 2 para True
update (KeyPress "O") battleSpace   = battleSpace { player2 = (player2 battleSpace) { isShooting = True } }
update (KeyRelease "O") battleSpace = battleSpace { player2 = (player2 battleSpace) { isShooting = False } }

--Botão R retorna o estado do mundo para o inicial (Reset), para facilitar os testes não sendo necessário compilar novamente
update (KeyPress "R") battleSpace  = worldInitial

update (TimePassing t) battleSpace  =
   outWorld t . attHP t . changeBullet t . navesMove t . collisionDetect . upRelogio t $ battleSpace 

update _ battleSpace  = battleSpace


-- Constantes -------------------------------------------------------------------------------------------------------
accNave :: Double
accNave = 1

--Velocidade do giro das naves
velAngNave :: Double
velAngNave = pi/2

worldLimit = 10

--Quantidade de tiros por segundo
cadArma = 6

--Velocidade "padrão" de saída do tiro (com a nave parada)
velSaidaArma :: Double
velSaidaArma = 3 

--Duração máxima do projétil, após a passagem desse tempo o projétil se auto destroi
durMaxProjetil = 5

--Tempo máximo que a nave consegue ficar fora do espaço visível
tempoMaxFora = 8

--Velocidade de rotação dos asteroides
velRotAsteroid = pi/10

--Tamanho do raio dos asteroides
raioAsteroid = 2

--Posição do centro dos asteroides
posAsteroid1 = (-6,6)
posAsteroid2 = (6,-6)


-- Funções de update -----------------------------------------------------------------------------------------------
--Pequeno relógio para controlar a visualização dos controles na tela (conta apenas até os 3 segs)
upRelogio t battle@BattleSpace { controlsTimer = clock } = battle { controlsTimer = if clock > 3 then 3 else clock + t }


--Movimentação da nave (uso de formula do movimento retilíneo acelerado, pos + vel * t + acc * t^2/2)
naveMove :: Double -> Point -> Vector -> Vector -> Point
naveMove t posXY acc vel = vectorSum posXY $ vectorSum (scaledVector t vel) (scaledVector (1/2 * t^2) acc)

--Velocidade da nave (velFinal = velInicial + acc * t)
newVelocity :: Double -> Vector -> Vector -> Vector
newVelocity t vel acc = vectorSum vel $ scaledVector t acc

--Controle da aceleração
acceleration :: Bool -> Direction -> Vector
--Caso esteja acelerando, multiplica o cos e o sin do angulo da direção da nave por uma constante
acceleration True dir = scaledVector accNave (cos dir, sin dir) 
--Caso não esteja acelerando, a aceleração é nula
acceleration False dir = (0,0) 

--Controle da variação da direção
varAng :: Double -> Rotation -> Direction -> Direction
varAng t CounterClock dir  = dir + velAngNave * t  --Se o estado do giro for anti horário soma com a velocidade do giro (constante)
varAng t ClockWise    dir  = dir - velAngNave * t  --Se o estado do giro for horário subtrai com a velocidade do giro (constante)
varAng _ Stop         dir  = dir                   --Se o estado do giro for parado não ocorre alterações

--Atualização da movimentação das naves
navesMove :: Double -> BattleWorld -> BattleWorld
navesMove t battle@BattleSpace 
  { player1 = SpaceShip { posShip = posP1,
                          velShip = velP1,
                          dirShip = dirP1,
                          rotShip = rotP1,
                          isAccele = isAccP1,
                          hpShip = hpP1
                        },
                     
    player2 = SpaceShip { posShip = posP2,
                          velShip = velP2,
                          dirShip = dirP2,
                          rotShip = rotP2,
                          isAccele = isAccP2,
                          hpShip = hpP2
                        },
    asteroid1 = Asteroid { pointsAste = pointsA1,
                           rotTimer = tRotacao1},
    asteroid2 = Asteroid { pointsAste = pointsA2,
                           rotTimer = tRotacao2}
  }
   = battle { player1 = (player1 battle) { posShip = naveMove t posP1 accP1 velP1,
                                           velShip = newVelocity t velP1 accP1,
                                           dirShip = dirHpCheck hpP1 t rotP1 dirP1},
                  
              player2 = (player2 battle) { posShip = naveMove t posP2 accP2 velP2,
                                           velShip = newVelocity t velP2 accP2,
                                           dirShip = dirHpCheck hpP2 t rotP2 dirP2},
            
              asteroid1 = Asteroid { pointsAste = asteroidRotation tRotacao1 posAsteroid1 pointsA1,
                                     rotTimer   = velRotAsteroid * (tRotacao1 + t)},
                           
              asteroid2 = Asteroid { pointsAste = asteroidRotation tRotacao2 posAsteroid2 pointsA2,
                                     rotTimer   = velRotAsteroid * (tRotacao2 + t)}
            }

      where
          --Mudança da aceleração
          accP1 = acceleration isAccP1 dirP1
          accP2 = acceleration isAccP2 dirP2
          
          dirHpCheck :: Int -> Double -> Rotation -> Direction -> Direction 
          dirHpCheck hp t rot dir 
             | hp <= 0   = dir                --Se o hp for menor que 0 a nave não pode se mexer(pois está "morta"), mantendo a direção sem alterações
             | otherwise = varAng t rot dir   --Caso contrário a direção é atualizada de acordo com o tempo
          
          --Função que controla a rotação do asteroide
          asteroidRotation :: Double -> Point -> Poligono -> Poligono
          asteroidRotation theta (posX,posY) = map (translatedPoint posX posY) . map (\(x,y) -> rotatedVector theta (x,y)) . coordOrigem 
            where
              coordOrigem poligono = map (\(x,y) -> (x - cX poligono, y - cY poligono)) poligono
 

          
--Atualizações dos projéteis
updateBullet :: Double -> Bullet -> [Bullet]
updateBullet t (Bullet pos@(x,y) v lifeTime)
  | lifeTime > durMaxProjetil  = []                     --Caso o tempo de vida seja maior que a duração máxima, o tiro é destruído 
  | x > worldLimit || x < -worldLimit ||                --Caso a posição do tiro ultrapasse os limites do mundo ele é destruído
    y > worldLimit || y < -worldLimit = []              
--Movimentação do tiro, soma da posição com a velocidade vezes o tempo, além disso, o tempo de vida é somado com o tempo corrente
  | otherwise = [Bullet (vectorSum pos $ scaledVector t v) v (lifeTime + t)]

--Criação de um novo projétil
newBullet :: Point -> Direction -> Vector -> Bullet
newBullet (x,y) dir vel
--É somado pelo seno/cosseno para mover o tiro para a ponta da nave, e multiplicado por 1.2 para evitar que os tiros provoquem dano a própria nave enquanto estão sendo gerados
  = Bullet (x+cos dir * 1.2, y+sin dir * 1.2) vel 0

--Velocidade tangencial caso a nave esteja girando para algum lado
velTangencial :: Direction -> Rotation -> Vector
velTangencial dir Stop         = (0,0)                                                --Parada não existe velocidade tangencial
velTangencial dir ClockWise    = scaledVector radius (cos $ dir-pi/2, sin $ dir-pi/2) --Rotacionando de forma horária é calculado o vetor perpendicular a direção (subtração)
velTangencial dir CounterClock = scaledVector radius (cos $ dir+pi/2, sin $ dir+pi/2) --Rotacionando de forma antihorária é calculado o vetor perpendicular a direção (adição)

--O raio é a distância entre a ponta da nave e o centroide
radius = vectorLength . vectorDifference (3.75,3.5) $ centroide naveCoord

--Mudança dos projetéis ao longo do tempo
changeBullet :: Double -> BattleWorld -> BattleWorld
changeBullet t battle@BattleSpace 
  { player1 = SpaceShip { posShip = posP1,
                          velShip = velP1,
                          dirShip = dirP1,
                          rotShip = rotP1,
                          isShooting = shooting1,
                          cooldownBullet = timerP1
                        },
                     
    player2 = SpaceShip { posShip = posP2,
                          velShip = velP2,
                          dirShip = dirP2,
                          rotShip = rotP2,
                          isShooting = shooting2,
                          cooldownBullet = timerP2
                        },
    projeteisP1 = bulletsP1,
    projeteisP2 = bulletsP2
  }
    --Caso esteja atirando e o clock da arma seja maior que 1/cad a nave está apta a gerar um novo projétil, caso contrário só atualiza os projéteis que já foram gerados
  = battle { projeteisP1 = if shooting1 && timerP1 >= 1/cadArma then attBullets t createBulletP1 else attBullets t bulletsP1,
             projeteisP2 = if shooting2 && timerP2 >= 1/cadArma then attBullets t createBulletP2 else attBullets t bulletsP2,
             player1 = (player1 battle) { cooldownBullet = if timerP1 > 1/cadArma then timerP1 - 1/cadArma else timerP1 + t }, --Atualização do timer que controla o tempo entre os tiros
             player2 = (player2 battle) { cooldownBullet = if timerP2 > 1/cadArma then timerP2 - 1/cadArma else timerP2 + t }
             }
          
               where 
                  --Criação de uma nova bala na cabeça da lista de balas
                  createBulletP1 = newBullet posP1 dirP1 velFinal1 : bulletsP1
                  createBulletP2 = newBullet posP2 dirP2 velFinal2 : bulletsP2
                  --Atualização de um único projétil aplicado na lista de projéteis
                  attBullets :: Double -> [Bullet] -> [Bullet]
                  attBullets t = concat . map (updateBullet t) 
                                
                  --A velocidade final da bala é a soma da velocidade da nave, velocidade de saida da arma e a velocidade tangencial
                  velFinal1 = vectorSum velP1 . vectorSum (velTangencial dirP1 rotP1) $ scaledVector velSaidaArma (cos dirP1, sin dirP1)
                  velFinal2 = vectorSum velP2 . vectorSum (velTangencial dirP2 rotP2) $ scaledVector velSaidaArma (cos dirP2, sin dirP2)
                                

--Função que checa todas as colisões
collisionDetect :: BattleWorld -> BattleWorld
collisionDetect = asteroidCollision . bulletCollision . navesCollision 

--Colisão entre projéteis e as naves
bulletCollision :: BattleWorld -> BattleWorld
bulletCollision battle@BattleSpace 
  { player1 = SpaceShip { posShip = posP1,
                          dirShip = dirP1,
                          hpShip  = hpP1
                        },
    
    player2 = SpaceShip { posShip = posP2,
                          dirShip = dirP2,
                          hpShip  = hpP2
                        },
    
    projeteisP1 = bullets1,
    projeteisP2 = bullets2
  }                                         --Caso um tiro da própria nave ou da nave adversária atinga a nave x ela perde 1 de vida
   = battle {  player1 = (player1 battle) { hpShip = if intersectBullet ship1Translated dirP2 bullets2 || 
                                                        intersectBullet ship1Translated dirP1 bullets1
                                                        then hpP1-1 
                                                        else hpP1},
               player2 = (player2 battle) { hpShip = if intersectBullet ship2Translated dirP1 bullets1 ||  
                                                        intersectBullet ship2Translated dirP2 bullets2
                                                        then hpP2-1 
                                                        else hpP2},
             
              --Para deletar os projéteis que entrarem em contato com a nave é necessário que a nave esteja viva, caso contrário (nave morta) não deleta
              projeteisP1 = if hpP2 > 0 then deleteColision ship1Translated dirP1 $ deleteColision ship2Translated dirP1 bullets1 
                                        else deleteColision ship1Translated dirP1 bullets1 ,
                           
              projeteisP2 = if hpP1 > 0 then deleteColision ship1Translated dirP2 $ deleteColision ship2Translated dirP2 bullets2 
                                        else deleteColision ship2Translated dirP2 bullets2 }
              where  
                  --Coordenadas da nave 1 no espaço (variando ao longo do tempo, de acordo com os controles)
                  ship1Translated = shipTranslated posP1 
                  --Coordenadas da nave 2 no espaço (variando ao longo do tempo, de acordo com os controles)
                  ship2Translated = shipTranslated posP2 
  
                  --Filtra as balas que não se intersectam com as naves, ou seja, quando o projétil se chocar com a nave ele é deletado
                  deleteColision :: Poligono -> Direction -> [Bullet] -> [Bullet]
                  deleteColision ship dir = filter (\bullet -> not $ intersectaPol (coordBullet dir bullet) ship) 
                                                   

--Checa se algum projétil se choca com um poligono
intersectBullet :: Poligono -> Direction -> [Bullet] -> Bool
intersectBullet poligono dir = any (\shot -> intersectaPol shot poligono) . map (coordBullet dir) 
                  
--Transformação da posição do projétil em um segmento levando em consideração o raio do círculo e uma margem de erro de 0.05
coordBullet dir (Bullet (x,y) _ _) = ((x+varX,y+varY), (x-varX,y-varY))
  where
     (varX, varY) = (0.1 * cos dir, 0.1 * sin dir)
     
--Pontos da nave em uma posição (x,y) no espaço
shipTranslated :: Point -> Poligono
shipTranslated (x,y) = 
  map (\(a,b) -> (a + x, b + y)) naveCoordOrigem        
    where
       --Coordenadas da nave transladadas para a origem
       naveCoordOrigem = map (\(x,y) -> (x - cX naveCoord, y - cY naveCoord)) naveCoord

--Caso as naves colidam entre si, ambas morrem instantaneamente
navesCollision battle@BattleSpace  
  { player1 = SpaceShip { posShip = posP1,
                          hpShip = hpP1,
                          explosionTime = expT1
                        },
    
    player2 = SpaceShip { posShip = posP2,
                          hpShip = hpP2,
                          explosionTime = expT2
                        }
  }                                  --É necessário checar o tempo de explosão da nave adversária para evitar que a nave morra ao colidir com a animação de explosão (que seria algo estranho)
   = battle { player1 = (player1 battle) { hpShip = if poligonosIntersectam (shipTranslated posP1) (shipTranslated posP2) && expT2 == 0
                                                       then 0 
                                                       else hpP1},
              player2 = (player2 battle) { hpShip = if poligonosIntersectam (shipTranslated posP1) (shipTranslated posP2) && expT1 == 0
                                                       then 0 
                                                       else hpP2}
              }
              

--Colisões envolvendo os asteroides (colisão com projéteis e com as naves)
asteroidCollision battle@BattleSpace  
  { player1 = SpaceShip { posShip = posP1,
                          dirShip = dirP1,
                          hpShip = hpP1
                        },
    
    player2 = SpaceShip { posShip = posP2,
                          dirShip = dirP2,
                          hpShip = hpP2
                        },
    
    projeteisP1 = bullets1,
    projeteisP2 = bullets2,
    asteroid1 = Asteroid { pointsAste = pointsA1},
    asteroid2 = Asteroid { pointsAste = pointsA2}
  }
   = battle { player1 = (player1 battle) { hpShip = if intersectAsteroid (shipTranslated posP1)
                                                       then 0                                  --Caso a nave bate no asteroide a vida dela é zerada instataneamente
                                                       else hpP1},
              player2 = (player2 battle) { hpShip = if intersectAsteroid (shipTranslated posP2)
                                                       then 0 
                                                       else hpP2},
              
              projeteisP1 = map (\bullet -> reflexBullet segAst1 dirP1 bullet) $ 
                            map (\bullet -> reflexBullet segAst2 dirP1 bullet) bullets1,      --Caso o projétil atinja o asteroide ele é refletido
              
              projeteisP2 = map (\bullet -> reflexBullet segAst1 dirP2 bullet) $ 
                            map (\bullet -> reflexBullet segAst2 dirP2 bullet) bullets2
              }
         where
             --Checa a colisão de uma nave com algum dos asteroides
             intersectAsteroid ship = poligonosIntersectam ship pointsA1 || poligonosIntersectam ship pointsA2 
             
             --Transformação dos pontos dos asteroides em segmentos
             segAst1 = distributiva pointsA1
             segAst2 = distributiva pointsA2
             
             --Calculo da bala refletida
             reflexBullet :: [Segmento] -> Direction -> Bullet -> Bullet
             reflexBullet [] _ bullet = bullet
             reflexBullet (seg:segs) dir bullet
               | seIntersectam seg (coordBullet dir bullet) = reflete seg bullet           --Caso o projétil se intersecte com algum lado do asteroide, ele é refletido 
               | otherwise                                  = reflexBullet segs dir bullet --Caso contrário a função é chamada novamente com outro segmento
                 where
                    --Chamada da reflexao do projétil (uso da função do projeto 1 aplicada no vetor velocidade do projétil)
                    reflete seg (Bullet pos vel lT) = (Bullet pos (refleteVetor vel seg) lT)
             
--Atualização das consequências envolvendo a vida das naves
attHP t battle@BattleSpace 
  { player1 = SpaceShip { posShip = posP1,
                          velShip = velP1,
                          isAccele = isAccP1,
                          isShooting = shooting1,
                          hpShip = hpP1,
                          explosionTime = expT1
                        },
                     
    player2 = SpaceShip { posShip = posP2,
                          velShip = velP2,
                          isAccele = isAccP2,
                          isShooting = shooting2,
                          hpShip = hpP2,
                          explosionTime = expT2
                        }
  }

   = battle { player1 = (player1 battle) { posShip       = positionGameOver hpP1 expT1 posP1,      
                                           explosionTime = timerExp hpP1 t expT1,
                                           velShip       = if hpP1 <= 0 then (0,0) else velP1,     --Caso a nave morra a velocidade é zerada
                                           isShooting    = if hpP1 <= 0 then False else shooting1, --Caso a nave morra é impossível continuar atirando
                                           isAccele      = if hpP1 <= 0 then False else isAccP1},  --Caso a nave morra o estado de aceleração também é falso
                                               
              player2 = (player2 battle) { posShip       = positionGameOver hpP2 expT2 posP2,
                                           explosionTime = timerExp hpP2 t expT2,
                                           velShip       = if hpP2 <= 0 then (0,0) else velP2,
                                           isShooting    = if hpP2 <= 0 then False else shooting2,
                                           isAccele      = if hpP2 <= 0 then False else isAccP2}
            }

--Calcula a posição de game over quando a nave morrer
positionGameOver :: Int -> Double -> Point -> Point
positionGameOver hp timeExp pos  
  | hp <= 0 && timeExp == 4 = (20,20) --Quando o hp for menor que zero e o tempo igual a 4 (sinalizando o término da explosão), a nave é movida para o (20,20)
  | otherwise               = pos
 
--Controla o timer da explosão
timerExp :: Int -> Double -> Double -> Double
timerExp hp time expTime
  | hp <= 0 && expTime <= 3 = expTime + time --Soma o timer com o tempo corrente
  | expTime > 3  = 4           --Caso a nave exploda será dado um tempo de 3 segs, depois disso o cronometro fixa em 4 (apenas um número simbólico para sinalizar o término)
  | otherwise    = 0           --Se o hp for diferente de 0, a nave não está em estado de explosão
                                
  
--Fora do espaço visível
outWorld t battle@BattleSpace 
  { player1 = SpaceShip { posShip = posP1,
                          timeOutWorld = timerP1,
                          hpShip = hpP1
                        },
                   
    player2 = SpaceShip { posShip = posP2,
                          timeOutWorld = timerP2,
                          hpShip = hpP2
                        }
  }                                        --Se a nave estiver morta o timer é desabilitado (travando no 0)
   = battle { player1 = (player1 battle) { timeOutWorld = if hpP1 <= 0 then 0 else countTimeOut posP1 t timerP1, 
                                           --Se o timer for maior que o tempoMaxFora a vida da nave é zerada instataneamente
                                           hpShip = if timerP1 > tempoMaxFora then 0 else hpP1},
                  
              player2 = (player2 battle) { timeOutWorld = if hpP2 <= 0 then 0 else countTimeOut posP2 t timerP2,
                                           hpShip = if timerP2 > tempoMaxFora then 0 else hpP2} }
     
--Contagem do tempo fora do espaço vísivel
countTimeOut :: Point -> Double -> Double -> Double
countTimeOut (x,y) t timer
  | x >= worldLimit || x <= -worldLimit ||
    y >= worldLimit || y <= -worldLimit     = timer + t 
  | otherwise                               = 0                           

--Criação do asteroid (poligono)       
createAsteroid :: Int -> Poligono
createAsteroid n  = 
  [ (raioAsteroid * cos (i * theta), raioAsteroid * sin (i*theta))| i <- [0 .. fromIntegral n - 1] ]
     where
        theta = 2*pi / fromIntegral n
        
        
-- Função de visualização --------------------------------------------------------------------------------------------------------------------
visualize ::  BattleWorld -> Picture
visualize battle@BattleSpace 
  { player1 = SpaceShip { posShip = (x1,y1),
                          dirShip = dirP1,
                          isAccele = accP1,
                          explosionTime = expT1,
                          hpShip = hpP1,
                          timeOutWorld = timer1
                        },
    
    player2 = SpaceShip { posShip = (x2,y2),
                          dirShip = dirP2,
                          isAccele = accP2,
                          explosionTime = expT2,
                          hpShip = hpP2,
                          timeOutWorld = timer2
                        },
   projeteisP1 = bullets1,
   projeteisP2 = bullets2,
   asteroid1 = Asteroid { pointsAste = pointsA1},
   asteroid2 = Asteroid { pointsAste = pointsA2},
   controlsTimer = clock
  }                     
     = infoControls clock &                                            --Informação dos controles
       translated (7.2) (-8) (timeOutCounting timer2) &                --Barra de visualização da contagem do tempo fora do espaço vísivel da nave 2
       translated (-7.7) (-8)(rotated pi (timeOutCounting timer1)) &   --Barra de visualização da contagem do tempo fora do espaço vísivel da nave 1
       explosion red (x1,y1) dirP1 expT1 &                             --Animação da explosão da nave 1
       explosion yellow (x2,y2) dirP2 expT2 &                          --Animação da explosão da nave 2
       translated (-9.5) (-9) (hpBar hpP1 red) &                       --Barra com a quantidade atual de vida da nave 1
       translated 4.5 (-9) (hpBar hpP2 yellow) &                       --Barra com a quantidade atual de vida da nave 2
       asteroidPic pointsA1 &                                          --Desenho do asteroide 1
       asteroidPic pointsA2 &                                          --Desenho do asteroide 2
       shotsPic red bullets1 &                                         --Projéteis da nave 1
       shotsPic yellow bullets2 &                                      --Projéteis da nave 2
       translated x1 y1 (rotated dirP1 (playerPic red accP1 hpP1)) &   --Desenho da nave 1
       translated x2 y2 (rotated dirP2(playerPic yellow accP2 hpP2)) & --Desenho da nave 2
       solidRectangle 30 30                                            --Plano de fundo do espaço
       

--Picture dos projéteis (círculos de diametro 0.1), coloridos de acordo com a cor do player
shotsPic :: Color -> [Bullet] -> Picture
shotsPic cor = pictures . map (\bullet -> translated (fst $ posBullet bullet) (snd $ posBullet bullet) . colored cor $ solidCircle 0.05)
              
--Picture do player, caso o hp seja menor igual a zero é desenhado um blank
playerPic :: Color -> Bool -> Int -> Picture
playerPic cor isAcc hp 
  | hp <= 0   = blank
  | otherwise = navePic cor & propulsao cor isAcc

--Picture da nave translatada para rotacionar de acordo com o seu centroíde
navePic :: Color -> Picture
navePic cor = rotated (-pi/2) $ translated (-cX naveCoord) (-cY naveCoord) $ colored cor $ thickPolygon 0.08 naveCoord

--Picture do asteroide
asteroidPic :: Poligono -> Picture
asteroidPic poligono  = colored white (thickPolygon 0.08 poligono) & solidPolygon poligono

--Coordenadas numéricas da nave 
naveCoord :: Poligono
naveCoord = [(3.75,3.5),(3,1.6),(3.2,1.8),(4.3,1.8),(4.5,1.6)]

--Picture da propulsão, recebe um booleano que controla a visualização (True = Visível, False = Invisível)
propulsao :: Color -> Bool -> Picture
propulsao cor isAcc = if isAcc then propPic else blank
    where
       propPic = rotated (-pi/2) . translated (-cX naveCoord) (-cY naveCoord) .
                 colored cor $ 
                 thickPolyline 0.08 [(3.5,1.8),(3.55,1.55),(3.6,1.65),(3.75,1.2),(3.9,1.65),(3.95,1.55),(4,1.8)]

--Picture da barra de visualização da vida das naves, recebe um inteiro que sinaliza a vida
hpBar :: Int -> Color -> Picture
hpBar n cor 
  | n <= 0    = hud cor & translated 2.25 (-0.1) . dilated 0.8 . colored white $ lettering "Game Over!" --Caso a vida zere, aparece um texto escrito Game Over
  | otherwise = hud cor & hp n 
  where
      --Quadrados verdes que sinalizam a quantidade de vida
      hp :: Int -> Picture
      hp 0 = blank
      hp n = colored green $ solidRectangle 0.5 0.8 & translated 0.5 0 $ hp (n-1)
      --Bordas coloridas de acordo com a coloração do player
      hud :: Color -> Picture
      hud cor = translated 2.25 0 . colored cor $ thickRectangle 0.06 5 0.8

--Picture da explosão, recebe um ponto de "partida", a rotação da imagem, e um timer para calcular o tempo decorrido
explosion :: Color -> Point -> Direction -> Double -> Picture
explosion cor (px,py) theta t 
--Caso o timer seja 0 é porque ainda não ocorreu a explosão, se for 4 é porque já ocorreu, em ambos os casos desenha um blank
  | t == 4 || t == 0 = blank 
  | otherwise = translated px py . rotated (theta-pi/2) $ linha1 & linha2 & linha3 & linha4 & pointsExp
    where
      linha1 
        | t > 3 = blank
        | otherwise = colored cor $ thickPolyline 0.08 [(0+t/5,-0.55-t/10),(0.5+t/5,-0.1-t/5)]
      linha2 
        | t > 1.5 = blank
        | otherwise = colored cor $ thickPolyline 0.08 [(0,-0.55-t/15),(-0.5-t/10,-0.1-t/10)] 
      linha3 
        | t > 2.8 = blank
        | otherwise = colored cor $ thickPolyline 0.08 [(0-t/5,0.8+t/10),(-0.6-t/10,-0.4+t/5)]
      linha4 
        | t > 2 = blank
        | otherwise = colored cor $ thickPolyline 0.08 [(0+t/50,0.8+t/5),(0.6+t/10,-0.4+t/4)] 
      pointsExp
        | t > 1 = blank
        | otherwise = pictures $ map (\dir -> rotated dir pic) [0,pi/6..2*pi]
           where
             points = map (\(x,y) -> (x*t*2,y*t*2)) lista
             lista =[(1,1),(-1,-1),(0.5,0.5), (-0.5,-0.5)]
             pic = pictures $ map (\(x,y) -> translated x y . colored cor $ solidCircle 0.05) points

--Visualização da barra que ilustra a contagem  do tempo fora do espaço visível
timeOutCounting :: Double -> Picture
timeOutCounting t 
--Caso o tempo seja maior que o tempoMaxFora  ou igual a 0 desenha um blank, pois ou a nave está dentro do espaço (tempo 0) 
--ou já está com tempo esgotado (tempo > tempoMaxFora) em ambos os casos a barra não precisa aparecer
  | t > tempoMaxFora  || t == 0 = blank                                    
--Caso contrário significa que a nave está fora do espaço visível, logo inicia-se a contagem (um retângulo que muda o comprimento de acordo com o tempo)
  | otherwise = translated (-2) 0 (solidRectangle t 0.6) & rec 
   where
       rec = colored blue $ solidRectangle 4 0.5

--Informações dos controles de jogabilidade na tela (visíveis por apenas 3 segundos)
infoControls t = if t >= 3 then blank else controlsP1 & controlsP2 & solidRectangle 20 20

controls a l r s cor = 
  texto & translated 4 (-1.5) (boxChar s cor) & 
  boxChar a cor & 
  translated (-1.5) (-1.5) (boxChar l cor) & 
  translated (1.5) (-1.5) (boxChar r cor) 
    where
      boxChar x cor = colored white (lettering x) & 
                      colored white (rectangle 1.5 1.5) & 
                      colored cor (solidRectangle 1.5 1.5) 
      texto = moves cor & shots cor
      moves cor = translated 0 1.5 . colored cor $ lettering "MOVES" 
      shots cor = translated 4 1.5 . colored cor $ lettering "SHOT"
--Controles player 1   
controlsP1 = translated (-6) 0 $ text & borda & controles 
  where
     controles = dilated 0.8 $ controls "W" "A" "D" "E" red
     borda = translated 1.25 0 . colored red $ rectangle 6.5 4.2
     text = translated 1.2 3 . colored red $ lettering "PLAYER 1"
--Controles player 2
controlsP2 = translated 3.5 0 $ text & borda & controles
  where
     controles = dilated 0.8 $ controls "I" "J" "L" "O" yellow
     borda = translated 1.25 0 . colored yellow $ rectangle 6.5 4.2
     text = translated 1.2 3 . colored yellow $ lettering "PLAYER 2"
       

distributiva :: Poligono -> [(Point, Point)]
distributiva poligono = zip poligono (drop 1 poligono ++ [head poligono]) 
area :: Poligono -> Double
area poligono  
    | calculaArea poligono < 0 = calculaArea poligono * (-1)
    | otherwise                = calculaArea poligono
        where
            calculaArea poligono = (sum [(x0*y1-x1*y0) | ((x0,y0),(x1,y1)) <- distributiva poligono ]) / 2
centroide :: Poligono -> (Point)
centroide poligono = (cX poligono, cY poligono)
cX :: Poligono -> Double
cX poligono = (sum[ (x0 + x1) * (x0 * y1 - x1 * y0) | ((x0,y0),(x1,y1)) <- distributiva poligono]) / (6 * area poligono)
cY :: Poligono -> Double
cY poligono = (sum[ (y0 + y1) * (x0 * y1 - x1 * y0) | ((x0,y0),(x1,y1)) <- distributiva poligono]) / (6*area poligono)

orientacao :: Point -> Point -> Point -> Int
orientacao (pX,pY) (qX,qY) (rX,rY) 
  |prodVetorial < 0 = -1
  |prodVetorial > 0 = 1
  |otherwise        = 0
  where
     prodVetorial = ((qY-pY)*(rX-qX)) - ((qX-pX)*(rY-qY))
dentroReta :: Point -> Point -> Point -> Bool
dentroReta (xP,yP) (x1,y1) (x2,y2)
  | (xP <= max x1 x2) && (xP >= min x1 x2) &&
    (yP <= max y1 y2) && (yP >= min y1 y2)     = True
  | otherwise                                  = False

seIntersectam :: Segmento -> Segmento -> Bool
seIntersectam ((p1), (q1)) ((p2), (q2))
  | orient1 /= orient2 && orient3 /= orient4  = True 
  | dentroReta p2 p1 q1 && orient1 == 0 = True
  | dentroReta q2 p1 q1 && orient2 == 0 = True
  | dentroReta p1 p2 q2 && orient3 == 0 = True
  | dentroReta q1 p2 q2 && orient4 == 0 = True
  | otherwise = False
    where
        orient1 = orientacao p1 q1 p2 
        orient2 = orientacao p1 q1 q2
        orient3 = orientacao p2 q2 p1
        orient4 = orientacao p2 q2 q1
intersectaPol :: Segmento -> Poligono -> Bool
intersectaPol ((p), (q)) poligono = 
    or [seIntersectam ((p),(q)) ((a),(b)) | ((a),(b)) <- distributiva poligono] 
poligonosIntersectam :: Poligono -> Poligono -> Bool
poligonosIntersectam poligono1 poligono2 = 
    or [seIntersectam ((p1),(q1)) ((p2),(q2)) | ((p1),(q1)) <- distributiva poligono1, ((p2),(q2)) <- distributiva poligono2]
    
refleteVetor :: Vector -> Segmento -> Vector
refleteVetor (vX,vY) ((x1,y1),(x2,y2)) = (reflectX,reflectY) 
    where 
       (reflectX,reflectY) = 
         (rotatedVector (2*(anguloSegmento-anguloVetor))  (vX,vY) )
       anguloVetor = vectorDirection (vX,vY)
       anguloSegmento = vectorDirection(x2-x1,y2-y1)