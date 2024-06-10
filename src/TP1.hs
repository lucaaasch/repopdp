module TP1 where
import PdePreludat
    ( otherwise,
      Eq((==)),
      Ord((<=), (>=)),
      Bool,
      String,
      Number,
      undefined,
      (&&),
      fromInteger,
      (+),(++),
      (-),error,mod )

esMes :: Number -> Bool
esMes numero = numero >=1 && numero <= 12

mesAnterior :: Number -> Number
mesAnterior 1 = 12
mesAnterior mesActual
    | esMes mesActual = mesActual-1
    | otherwise = error "El valor ingresado no es un mes"

mesSiguiente :: Number -> Number
mesSiguiente 12 = 1
mesSiguiente mesActual
    | esMes mesActual = mesActual+1
    | otherwise = error "El valor ingresado no es un mes"

-- Todas estas funciones son abstracciones de la funcion estacion

-- Sirve para verificar si "num" es multiplo de "div"
esMultiploDe :: Number -> Number -> Bool
esMultiploDe num div = num `mod` div == 0 

-- Esta función devuelve la estación del mes anterior al actual, se implementa en ambos casos
estacionAnterior :: Number -> String
estacionAnterior mesActual = estacion (mesAnterior mesActual)

-- Esta función devuelve la estación del mes siguiente al actual, sirve para los casos de meses divisibles por 3
estacionSiguiente :: Number -> String
estacionSiguiente mesActual = estacion (mesSiguiente mesActual)

concatenarEstaciones :: Number -> String
concatenarEstaciones mesActual = estacionAnterior mesActual ++ "/" ++ estacionSiguiente mesActual

esDual :: Number -> Bool
esDual mesActual = esMes mesActual && mesActual `esMultiploDe` 3

-------------------------------------------------------------------

estacion :: Number -> String
estacion 1 = "verano"
estacion 4 = "otonio"
estacion 7 = "invierno"
estacion 10 = "primavera"
estacion mesActual
    | esDual mesActual = concatenarEstaciones mesActual
    | otherwise = estacionAnterior mesActual