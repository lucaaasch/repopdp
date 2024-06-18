module TP2 where
import PdePreludat

data Ficha = Ficha {
  izquierdo :: Number,
  derecho :: Number
} deriving (Show, Eq)

type Tablero = (Ficha, Ficha)

-- Funciones para incrementar la expresividad de las fichas del tablero 

fichaIzquierda :: Tablero -> Ficha
fichaIzquierda = fst

fichaDerecha :: Tablero -> Ficha
fichaDerecha = snd

-----------------------------------------------------------------------

invertirFicha :: Ficha -> Ficha
invertirFicha ficha = Ficha {izquierdo = derecho ficha, derecho = izquierdo ficha}

esLaMismaFicha :: Ficha -> Ficha -> Bool
esLaMismaFicha fichaUno fichaDos = fichaUno == fichaDos || fichaUno == invertirFicha fichaDos

-- Abstracciones de la función jugarRonda ---------------------------------------------------------------------

-- Las conexiones pueden ser por ambos extremos:

seConectanPorIzquierda :: Ficha -> Ficha -> Bool
seConectanPorIzquierda fichaUno fichaDos = derecho fichaUno == izquierdo fichaDos

seConectanPorDerecha :: Ficha -> Ficha -> Bool
seConectanPorDerecha fichaUno fichaDos = izquierdo fichaUno == derecho fichaDos

-- La funcion conectarConTablero se encarga de verificar si una ficha se puede conectar al tablero, si puede la conecta.

conectarConTablero :: Ficha -> Tablero -> Tablero 
conectarConTablero ficha tablero

-- En caso de que se conecten por izquierda sin necesidad de invertir la ficha
  | seConectanPorIzquierda ficha (fichaIzquierda tablero) = (ficha,fichaDerecha tablero)

-- En caso de que se conecten por izquierda con necesidad de invertir la ficha
  | seConectanPorIzquierda (invertirFicha ficha) (fichaIzquierda tablero) = (invertirFicha ficha,fichaDerecha tablero)

-- En caso de que se conecten por derecha sin necesidad de invertir la ficha
  | seConectanPorDerecha ficha (fichaDerecha tablero) = (fichaIzquierda tablero,ficha)

-- En caso de que se conecten por derecha con necesidad de invertir la ficha
  | seConectanPorDerecha (invertirFicha ficha) (fichaDerecha tablero) = (fichaIzquierda tablero, invertirFicha ficha)

-- Si no se conectan, el tablero queda como estaba
  | otherwise = tablero

---------------------------------------------------------------------------------------------------------------

jugarRonda :: Ficha -> Ficha -> Tablero -> Tablero
jugarRonda fichaUno fichaDos = conectarConTablero fichaDos . conectarConTablero fichaUno