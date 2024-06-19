-- Mariano Pessina
module Hierbas where

import Text.Show.Functions
import Data.List

type Nombre = String
type Edad = Float
type Peso = Float
type Enfermedades = [String]

data Raton = UnRaton {
    nombre :: Nombre,
    edad :: Edad,
    peso :: Peso,
    enfermedades :: Enfermedades
    } deriving (Show)

--modelado de ratones

cerebro :: Raton
cerebro = UnRaton {
    nombre = "cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata  :: Raton
bicenterrata  = UnRaton {
    nombre = "bicenterrata ",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo  :: Raton
huesudo  = UnRaton {
    nombre = "huesudo ",
    edad = 4,
    peso = 10,
    enfermedades = ["altaObesidad","sinusitis"]
}

--modelado de hierbas

type Hierba = (Raton -> Raton)

hierbaBuena :: Hierba
hierbaBuena raton = rejuvenecer (sqrt (edad raton)) raton

rejuvenecer :: Edad -> Raton -> Raton
rejuvenecer anios raton = raton {edad = anios}

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = filter (not.terminaCon terminacion) (enfermedades raton)}

terminaCon :: String -> String -> Bool
terminaCon terminacion enfermedad = terminacion == drop (length enfermedad - length terminacion) enfermedad

alcachofa :: Hierba
alcachofa raton = perderPeso (peso raton * coeficiente raton) raton

coeficiente :: Raton -> Float
coeficiente raton | (peso raton) > 2 = 0.1
                  | otherwise = 0.05

perderPeso :: Peso -> Raton -> Raton
perderPeso pesoAPerder raton = raton {peso = max (peso raton - pesoAPerder) 0}

hierbaZort :: Hierba
hierbaZort = cambiarNombreA "pinky" . rejuvenecer 0 . perderEnfermedades

perderEnfermedades :: Raton -> Raton
perderEnfermedades raton = raton {enfermedades = []}

cambiarNombreA :: Nombre -> Raton -> Raton
cambiarNombreA nuevoNombre raton = raton {nombre = nuevoNombre}

hierbaDelDiablo :: Hierba
hierbaDelDiablo = perderPeso 0.1 . eliminarEnfermedades 10 

eliminarEnfermedades :: Int -> Raton -> Raton
eliminarEnfermedades cantidadDeLetrasMax raton = raton {enfermedades = filter ((<=cantidadDeLetrasMax).length) (enfermedades raton)}

--medicamentos
type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = componer [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

reduceFatFast :: Int -> Medicamento
reduceFatFast = componer . listaDeHierbas

listaDeHierbas :: Int -> [Hierba]
listaDeHierbas potencia = replicate potencia alcachofa ++ [hierbaVerde "Obesidad"]

pdepCilina :: Medicamento
pdepCilina = componer (map hierbaVerde sufijosInfecciosas)

sufijosInfecciosas :: [String]
sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

componer :: [Hierba] -> Medicamento
componer [] = id
componer (x:xs) = x . componer xs
-- 4 experimentos 
--a
cantidadIdeal :: (Num a, Enum a) => (a -> Bool) -> a
cantidadIdeal condicion = head (filter condicion [1..])
--b

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = ningunoConSobrepeso (medicarRatones medicamento ratones) && menosDe3Enfermedades (medicarRatones medicamento ratones)

medicarRatones :: Medicamento -> [Raton] -> [Raton]
medicarRatones = map

ningunoConSobrepeso :: [Raton] -> Bool
ningunoConSobrepeso ratones = all ((<1).peso) ratones

menosDe3Enfermedades :: [Raton] -> Bool
menosDe3Enfermedades ratones = all ((<3).length.enfermedades) ratones

--c
experimento :: [Raton] -> Int
experimento comunidad = cantidadIdeal (\potencia -> lograEstabilizar (reduceFatFast potencia) comunidad) 

--5
{-a
No podremos saber si se estabiliza esa comunidad en el caso a). Cuando la comunidad sea infinita y 
todos los ratones cumplan las condiciones, lograEstabilizar se quedará "colgada" sin dar ningún 
resultado. Eso es porque ningunoConSobrepeso utiliza el all, que nunca dejará de probar a menos 
que encuentre un caso falso.
-}

{-b
Sí podremos saber si se estabiliza la comunidad en el caso b). Cuando la comunidad tenga un ratón
que no cumple las condiciones de estabilización, lograEstabilizar retornará False. Eso es porque
ambos all de ningunoConSobrepeso y menosDe3Enfermedades terminan y dan False, porque el mecanismo
de Lazy Evaluation (evaluación diferida) le permite a Haskell no evaluar toda la lista para darse cuenta.
-}

--6
{-a
Simplemente hay que agregar la hierba, no es necesario cambiar ninguna de las otras funciones.
Debe respetar el tipo Hierba, y si respeta ese tipo puede utilizarse en funciones como la función "componer".
-}

{-b
El concepto que está involucrado en la pregunta anterior es el de TAD, en donde se toma a la Hierba
como un valor, y en particular la función "componer" como una primitiva. Si cambia el modelado de las 
hierbas, no hace falta cambiar ninguno de los medicamentos.
-}

{-c
Si se quiere poner el peso en libras hay que cambiar todas las funciones que hagan cálculos o comparaciones
con el peso ya que están puestas en kg.
Ej: si un ratón pesa más de 2kg, hay que transformar el 2 a libras.
-}