import PdePreludat
    ( otherwise,
      Enum,
      Eq((/=)),
      Ord((>), (>=), (<=), max),
      Show,
      Bool,
      String,
      Number,
      foldl1,
      (-),
      (++),
      (.),
      elem,
      (*),
      length,
      head,
      filter,
      error,
      undefined,
      (&&),
      fromInteger,
      (...),
      ($),
      (/), takeWhile,map,snd,fst)
import Data.List (sortBy)

-- Modelo inicial
data Jugador = Jugador {
nombre :: String,
padre :: String,
habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
fuerzaJugador :: Number,
precisionJugador :: Number
} deriving (Eq, Show)

habilidadBart :: Habilidad
habilidadBart = Habilidad {fuerzaJugador = 25, precisionJugador = 60}

habilidadTodd :: Habilidad
habilidadTodd = Habilidad {fuerzaJugador = 15, precisionJugador = 80}

habilidadRafa :: Habilidad
habilidadRafa = Habilidad {fuerzaJugador = 10, precisionJugador = 1}

-- Jugadores de ejemplo
bart :: Jugador
bart = Jugador {nombre = "Bart", padre = "Homero", habilidad = habilidadBart}

todd :: Jugador
todd = Jugador {nombre = "Todd", padre = "Ned", habilidad = habilidadTodd}

rafa :: Jugador
rafa = Jugador {nombre = "Rafa", padre = "Gorgory", habilidad = habilidadRafa}

data Tiro = Tiro {
velocidad :: Number,
precision :: Number,
altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

modificarVelocidad :: Number  -> Tiro -> Tiro
modificarVelocidad n tiro = tiro {velocidad = n}

modificarVelocidad' :: (Number -> Number) -> Tiro -> Tiro
modificarVelocidad' func tiro = tiro {velocidad = func (velocidad tiro)}

modificarPrecision :: Number -> Tiro -> Tiro
modificarPrecision n tiro = tiro {precision = n}

modificarPrecision' :: (Number -> Number) -> Tiro -> Tiro
modificarPrecision' func tiro = tiro {precision = func (precision tiro)}

modificarAltura :: Number  -> Tiro -> Tiro
modificarAltura n tiro = tiro {altura = n}

modificarAltura' :: (Number -> Number) -> Tiro -> Tiro
modificarAltura' func tiro = tiro {altura = func (precision tiro)}

type Palo = Habilidad -> Tiro -> Tiro

putter :: Palo
putter habilidad  = modificarAltura 0 . modificarPrecision (2* precisionJugador habilidad) . modificarVelocidad 10

madera :: Palo
madera habilidad = modificarPrecision (precisionJugador habilidad / 2) . modificarAltura 5 . modificarVelocidad 100

hierros :: Number -> Palo
hierros n habilidad
    | n >= 1 && n <= 10 = modificarAltura (max 0 $ n - 3) . modificarPrecision (precisionJugador habilidad / n) . modificarVelocidad (fuerzaJugador habilidad * n)
    | otherwise = error "Hierros solo va del 1 al 10"

palos :: [Palo]
palos = [putter, madera] ++ [hierros 1 ... hierros 10]

tiroDefault :: Tiro
tiroDefault = Tiro {velocidad = 0, precision = 0, altura = 0}

type UsarPalo = Jugador -> (Palo) -> Tiro

golpe :: UsarPalo
golpe jugador palo = palo (habilidad jugador) tiroDefault

type Obstaculo = Tiro -> Tiro

superaTunel :: Tiro -> Bool
superaTunel tiro = precision tiro > 90

tunel :: Obstaculo
tunel = superarObstaculo (modificarPrecision 100 . modificarAltura 0) superaTunel

superaLaguna :: Number -> Tiro -> Bool
superaLaguna largo tiro = velocidad tiro > 80 && between 1 5 largo

laguna :: Number -> Obstaculo
laguna largo tiro = superarObstaculo (modificarAltura (altura tiro / largo)) (superaLaguna largo) tiro

superaHoyo :: Tiro -> Bool
superaHoyo tiro = between 5 20 (velocidad tiro)

hoyo :: Obstaculo
hoyo = superarObstaculo reiniciarTiro superaHoyo 
    where reiniciarTiro = modificarAltura 0 . modificarPrecision 0 . modificarVelocidad 0

superarObstaculo :: (Obstaculo)  -> (Tiro -> Bool) -> Tiro -> Tiro
superarObstaculo func condicion tiro
    | condicion tiro = func tiro
    | otherwise = tiroDefault -- Vamos a asumir que si el tiro no se puede hacer, queda en 0 (todos sus campos)

-- Punto 4.a

superaObstaculoPalo :: Jugador -> Obstaculo -> Palo -> Bool
superaObstaculoPalo jugador obstaculo palo = obstaculo (golpe jugador palo) /= tiroDefault

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculoPalo jugador obstaculo) palos

-- Punto 4.b

superaObstaculoTiro :: Tiro -> Obstaculo -> Bool
superaObstaculoTiro tiro obstaculo = obstaculo tiro /= tiroDefault

filtrarConsecutividad :: Tiro -> [Obstaculo] -> [Obstaculo]
filtrarConsecutividad _ [] = []
filtrarConsecutividad tiro (cab : cola)
    | superaObstaculoTiro tiro cab = cab : filtrarConsecutividad tiro cola

obstaculosConsecutivos :: Tiro -> [Obstaculo] -> Number
obstaculosConsecutivos tiro = length . filtrarConsecutividad tiro 

-- BONUS: Sin recursividad
obstaculosConsecutivosB :: Tiro -> [Obstaculo] -> Number
obstaculosConsecutivosB tiro = length . takeWhile (superaObstaculoTiro tiro)

-- Punto 4.c 

cantidadObstaculos:: Tiro -> [Obstaculo] -> Number
cantidadObstaculos tiro = length . filter (superaObstaculoTiro tiro)

type UsarPalo' = Jugador -> (Palo) -> Tiro

golpe' :: UsarPalo
golpe' jugador palo = palo (habilidad jugador) tiroDefault


-- Función que cuenta cuántos obstáculos supera un palo

-- Levemente modificado su TIPADO para que Haskell no llore :C
superaObstaculoConPalo :: Jugador -> Palo -> Obstaculo -> Bool
superaObstaculoConPalo jugador palo obstaculo = obstaculo (golpe jugador palo) /= tiroDefault

obstaculosSuperadosConPalo :: Jugador -> Palo -> [Obstaculo]  -> Number
obstaculosSuperadosConPalo jugador  palo = length . filter (superaObstaculoConPalo jugador palo) 

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (\p -> obstaculosSuperadosConPalo jugador p obstaculos) palos

-- Punto 5

type Padre = String

ganador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
ganador = maximoSegun snd

listaNiños :: [(Jugador, Puntos)]
listaNiños = [(bart, 10), (todd, 20), (rafa, 5)]

padresPerdedores :: [(Jugador, Puntos)] -> [Padre]
padresPerdedores lista = map (padre . fst) (filter (/= ganador lista) lista)