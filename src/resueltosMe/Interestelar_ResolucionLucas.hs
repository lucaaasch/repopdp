import PdePreludat

data Planeta = Planeta{
    nombre :: String,
    posicion :: Posicion,
    relacion :: Relacion
} deriving(Show,Eq)

type Relacion = (Number -> Number)

data Posicion = Posicion {
    x :: Number,
    y :: Number,
    z :: Number
} deriving(Show,Eq)

data Astronauta = Astronauta {
  nombreAstronauta :: String,
  edad :: Number,
  planeta :: Planeta
}

posicionMarte :: Posicion
posicionMarte = Posicion 3 4 5

marte :: Planeta
marte = Planeta {nombre = "Marte",posicion = posicionMarte ,relacion = (+1)}

square :: Number -> Number
square n = n * n

sumarCoordenadas :: Planeta -> Planeta -> Number
sumarCoordenadas p1 p2 = square restaX + square restaY + square restaZ
    where restaX = x (posicion p1) - x (posicion p2)
          restaY = y (posicion p1) - y (posicion p2)
          restaZ = z (posicion p1) - z (posicion p2)

distanciaEntreDosPlanetas :: Planeta -> Planeta -> Number
distanciaEntreDosPlanetas p1 = sqrt . sumarCoordenadas p1

tiempoViaje ::  Number -> Planeta -> Planeta -> Number
tiempoViaje velocidad p1 = flip div velocidad . distanciaEntreDosPlanetas p1

pasarTiempo :: Number -> Planeta -> Astronauta -> Astronauta
pasarTiempo tiempo planeta astronauta = astronauta {edad = relacion planeta tiempo}

type Nave = Planeta -> Planeta -> Number

naveVieja :: Number -> Nave
naveVieja tanques  
    | tanques < 6 = tiempoViaje 10 
    | otherwise = tiempoViaje 7

naveFuturista :: Nave
naveFuturista _ _ = 0

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta nuevoPlaneta astronauta = astronauta {planeta = nuevoPlaneta}

viajar :: Planeta -> Planeta -> (Nave) -> Astronauta -> Astronauta
viajar origen destino nave = cambiarPlaneta destino . pasarTiempo (nave origen destino) destino

viajarTodos :: Planeta -> Planeta -> (Nave) -> [Astronauta] -> [Astronauta]
viajarTodos origen destino nave = map (viajar origen destino nave)

modificarRescatado :: (Nave) -> Planeta -> Planeta -> Astronauta -> Astronauta
modificarRescatado nave origen destino = pasarTiempo (nave origen destino) destino

agregarAstronauta :: Astronauta -> [Astronauta] -> [Astronauta]
agregarAstronauta astronauta = flip (++) [astronauta]

planetaDestino :: [Astronauta] -> Planeta
planetaDestino = planeta . head

rescate :: Astronauta -> (Nave) -> [Astronauta] -> [Astronauta]
rescate astronauta nave grupo =  viajeVuelta $ sumarTripulante viajeIda
    where viajeIda = viajarTodos (planetaDestino grupo) (planeta astronauta) nave grupo
          sumarTripulante = agregarAstronauta (modificarRescatado nave (planetaDestino grupo) (planeta astronauta) astronauta)
          viajeVuelta = viajarTodos (planeta astronauta) (planetaDestino grupo) nave 

estaViejo :: Astronauta -> Bool
estaViejo astronauta = edad astronauta > 90

puedeSerRescatado :: Nave -> [Astronauta] -> Astronauta -> Bool
puedeSerRescatado nave grupo varado =  not . any estaViejo . rescate varado nave $ grupo

nombresRescatables :: Nave -> [Astronauta] -> [Astronauta] -> [String]
nombresRescatables nave grupoRescatistas = map nombreAstronauta . filter (puedeSerRescatado nave grupoRescatistas)

-- Punto 5

f :: Ord t => (t -> a -> t) -> t -> (Number -> a -> Bool) -> [a] -> Bool
f a b c = any ((> b).a b).filter (c 10)