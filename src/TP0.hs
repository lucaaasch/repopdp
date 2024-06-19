module TP0 where

import PdePreludat
data Perrito = Perrito{
    raza :: String,
    juguetes :: [String],
    tiempo :: Number, -- En minutos
    energia :: Number
} deriving (Show,Eq)

data Guarderia = Guarderia{
    nombre :: String,
    rutina :: [Actividad]
} deriving (Show,Eq)

type Ejercicio = Perrito -> Perrito

data Actividad = Actividad{
    ejercicio :: Ejercicio,
    tiempoEjercicio :: Number
}deriving (Show,Eq)

modificarEnergia :: (Number -> Number) -> Perrito -> Perrito
modificarEnergia cambio perrito = perrito {energia = cambio (energia perrito)}

jugar :: Ejercicio
jugar = modificarEnergia (max 0 . subtract 10)

ladrar :: Number -> Ejercicio
ladrar ladridos = modificarEnergia (+ladridos)

modificarJuguetes :: ([String] -> [String]) -> Perrito -> Perrito
modificarJuguetes cambio perrito = perrito { juguetes = cambio (juguetes perrito)}

regalar :: String -> Ejercicio
regalar juguete = modificarJuguetes (++[juguete])

esExtravagante :: Perrito -> Bool
esExtravagante perrito = raza perrito == "Pomerania" || raza perrito == "Dálmata"

diaDeSpa :: Ejercicio
diaDeSpa perrito
    | esExtravagante perrito || tiempo perrito > 50 = modificarEnergia (min 100 . subtract (energia perrito)) . regalar "Peine de goma" $ perrito
    | otherwise = perrito

diaDeCampo :: Ejercicio
diaDeCampo = modificarJuguetes (drop 1)

zara :: Perrito
zara = Perrito {raza = "Dálmata", juguetes = ["Pelota","Mantita"], tiempo = 90, energia = 80}

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia {nombre = "Guarderia PdePerritos",rutina = [Actividad jugar 30, Actividad (ladrar 18) 20, Actividad (regalar "Pelota") 0, Actividad diaDeSpa 120, Actividad diaDeCampo 720]}

tiempoGuarderia :: Guarderia -> Number
tiempoGuarderia guarderia = sum . map tiempoEjercicio $ rutina guarderia

puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia perrito guarderia = tiempo perrito < tiempoGuarderia guarderia

esResponsable :: Perrito -> Bool
esResponsable = (>3) . length . juguetes . diaDeCampo 

aplanarActividades :: [Ejercicio] -> Ejercicio
aplanarActividades = foldr (.) id

conseguirRutina :: Guarderia -> Ejercicio
conseguirRutina = aplanarActividades . map ejercicio . rutina

todaLaRutina :: Guarderia -> Ejercicio
todaLaRutina guarderia perrito
    | tiempo perrito >= tiempoGuarderia guarderia = conseguirRutina guarderia perrito
    | otherwise = perrito

estaCansado :: Perrito -> Bool
estaCansado perrito = energia perrito < 5

perritosCansados :: Guarderia -> [Perrito] -> [Perrito]
perritosCansados guarderia = filter estaCansado . map (todaLaRutina guarderia)

perritoPi :: Perrito
perritoPi = Perrito {raza = "Labrador", juguetes = [show n | n <- [1..]], tiempo = 314, energia = 159}

-- Parte C

{-
    1) Si, es posible : > esExtravagante perritoPi 
    2) Respecto a los puntos a y b NO. Nosotros podemos saber que no lo tienen (pues lo podemos ver en la declaracion de perritoPi) pero la computadora tiene que iterar toda la lista para saberlo (y al ser infinita, nunca se produce resultado).
    Respecto del punto C, si porque se tienen que realizar 31112 iteraciones hasta llegar a esa soga y bueno, se encuentra esa ocurrencia y se devuelve True (tarda pero llega)
    3) Depende, si la rutina implica actividades de regalar un juguete NO, pero si no se toca la lista de juguetes SI.
    4) Simplemente no se puede.

-}
