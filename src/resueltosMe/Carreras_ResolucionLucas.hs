import PdePreludat

type Color = String
data Auto = Auto{
    color :: Color,
    velocidad :: Number,
    distancia :: Number
} deriving (Show,Eq)

type Carrera = [Auto]

mario :: Auto
mario = Auto {color = "Rojo", velocidad = 100, distancia = 1000}

luigi :: Auto
luigi = Auto {color = "Verde Claro", velocidad = 70, distancia = 980}

yoshi :: Auto
yoshi = Auto {color = "Verde Oscuro", velocidad = 90, distancia = 700}

peach :: Auto
peach = Auto {color = "Rosa", velocidad = 40, distancia = 350}

marioGoat :: Carrera
marioGoat = [luigi,yoshi,peach,mario]

-- Punto 1

-- Punto 1.a
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaEntreAutos < 10
    where distanciaEntreAutos = abs (distancia auto1 - distancia auto2)

leGana :: Auto -> Auto -> Bool
leGana auto1 auto2 = distancia auto1 > distancia auto2

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = all (leGana auto) . filter (/= auto)

vaSolo :: Auto -> Carrera -> Bool
vaSolo auto = not . any (estaCerca auto)

-- Punto 1.b
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = vaSolo auto carrera && vaGanando auto carrera 

-- Punto 1.c

puesto :: Auto -> Carrera -> Number
puesto auto = length . filter (not . leGana auto)

-- Punto 2

-- Punto 2.a

correr :: Number -> Auto -> Auto
correr tiempo auto = auto {distancia = (distancia auto + tiempo) * velocidad auto}

-- Punto 2.b

modificarVelocidad :: (Number -> Number) -> Auto -> Auto
modificarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto)}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad aBajar = modificarVelocidad (max 0 . subtract aBajar)

-- Punto 3

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type Powerup = Auto -> Carrera -> Carrera

-- Punto 3.a
terremoto :: Powerup
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50)

-- Punto 3.b
miguelitos :: Number -> Powerup
miguelitos cantMiguelitos auto = afectarALosQueCumplen (not . leGana auto) (bajarVelocidad cantMiguelitos)

-- Punto 3.c
usarJetpack :: Number -> Auto -> Auto
usarJetpack tiempo auto = bajarVelocidad (velocidad auto) . correr tiempo . modificarVelocidad (*2) $ auto

jetpack :: Number -> Powerup
jetpack tiempoVuelo auto = afectarALosQueCumplen (== auto) (usarJetpack tiempoVuelo)

-- Punto 4

-- Punto 4.a

type Evento = Carrera -> Carrera
type Puesto = Number

tuplaGana :: (Puesto, Color) -> (Puesto, Color) -> Bool
tuplaGana tupla1 tupla2 = fst tupla1 > fst tupla2

aplanarEventos :: [Evento] -> Evento
aplanarEventos = foldr (.) id

aplicarEventos :: [Evento] -> Carrera -> Carrera
aplicarEventos = aplanarEventos  

armarTabla :: Carrera -> [(Number, Color)]
armarTabla carrera = map (conseguirTupla carrera) carrera

conseguirTupla :: Carrera -> Auto -> (Number, String)
conseguirTupla carrera auto = (puesto auto carrera, color auto)

simularCarrera ::  [Evento] -> Carrera -> [(Puesto, Color)]
simularCarrera eventos = armarTabla . aplicarEventos eventos 

-- Punto 4.b

correnTodos :: Number -> Evento
correnTodos tiempo = map (correr tiempo)

find :: (c -> Bool) -> [c] -> c
find cond = head . filter cond

usaPowerUp :: Powerup -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera = powerUp autoBuscado carrera
    where autoBuscado = find ((== colorBuscado) . color) carrera

-- Punto 4.c

autoAzul :: Auto
autoAzul = Auto {color = "Azul", velocidad = 120, distancia = 0}

autoRojo :: Auto
autoRojo = Auto {color = "Rojo", velocidad = 120, distancia = 0}

autoBlanco :: Auto
autoBlanco = Auto {color = "Blanco", velocidad = 120, distancia = 0}

autoNegro :: Auto
autoNegro = Auto {color = "Negro", velocidad = 120, distancia = 0}

carreraColorida :: Carrera
carreraColorida = [autoAzul,autoRojo,autoBlanco,autoNegro]

simularCarrera4C :: [(Puesto, Color)]
simularCarrera4C = simularCarrera [correnTodos 30, usaPowerUp (jetpack 3) (color autoAzul), usaPowerUp terremoto (color autoBlanco), correnTodos 40,  usaPowerUp (miguelitos 20) (color autoBlanco), usaPowerUp (jetpack 6) (color autoNegro), correnTodos 10] carreraColorida

-- Punto 5.a
{- 

Si, se puede hacer. El prototipo de la funcion seria algo como esto:

misil :: Color -> Powerup
misil color auto carrera = undefined

-}

-- Punto 5.b
{- 

vaTranquilo puede terminar sólo si el auto indicado no va tranquilo
(en este caso por tener a alguien cerca, si las condiciones estuvieran al revés, 
terminaría si se encuentra alguno al que no le gana).
Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla 
la condición indicada, y all es capaz de retornar False si alguno no cumple la condición correspondiente. 
Sin embargo, no podría terminar si se tratara de un auto que va tranquilo.

- puesto no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más 
que se pueda tratar de filtrar el conjunto de autos, nunca se llegaría al final para calcular la longitud.

-}

