import Text.Show.Functions
import Data.Char

type Produccion = Serie -> Serie

data Serie = Serie {
    nombre :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadas :: Int,
    rating :: Int,
    cancelada :: Bool
} deriving Show

data Actor = Actor {
    nombreActor :: String,
    sueldo :: Int,
    restricciones :: [String]
} deriving Show

-- Punto 1

estaEnRojo :: Serie -> Bool
estaEnRojo serie = (presupuesto serie) > (cobranActores (actores serie))

cobranActores :: [Actor] -> Int
cobranActores = sum . map sueldo

esProblematica :: Serie -> Bool
esProblematica = (>3) . (tieneRestricciones 1)

tieneRestricciones :: Int -> Serie -> Int
tieneRestricciones num serie = length $ filter (masDeXRestricciones num) (actores serie)

masDeXRestricciones num = (>num) . length . restricciones 

-- Punto 2

conFavoritismo :: [Actor] -> Produccion
conFavoritismo actoresFavoritos = reemplazarActores actoresFavoritos . (eliminarActores 2)

mapActoresSerie :: ([Actor] -> [Actor]) -> Serie -> Serie
mapActoresSerie funcion serie = serie { actores = funcion (actores serie) }

eliminarActores :: Int -> Serie -> Serie
eliminarActores num = mapActoresSerie (drop num)

reemplazarActores :: [Actor] -> Serie -> Serie
reemplazarActores actoresFavoritos = mapActoresSerie (++ actoresFavoritos)

timBurton = conFavoritismo [johnnyDepp, helenaBonhamCarter]

johnnyDepp :: Actor
johnnyDepp = Actor "johnny depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "helema bonham carter" 15000000 []

gatopardeitor :: Produccion
gatopardeitor serie = serie

estireitor :: Produccion
estireitor = mapTemporadas (*2)

mapTemporadas :: (Int -> Int) -> Serie -> Serie
mapTemporadas funcion serie = serie { temporadas = funcion (temporadas serie) }

desespereitor :: Produccion
desespereitor = gatopardeitor . estireitor

canceleitor :: Int -> Produccion
canceleitor cifra serie
  | estaEnRojo serie || (rating serie) > cifra = mapCancelada (const True) serie
  | otherwise = serie

mapCancelada :: (Bool -> Bool) -> Serie -> Serie
mapCancelada funcion serie = serie { cancelada = funcion (cancelada serie) }

-- Punto 3

bienestarLongitud :: Serie -> Int
bienestarLongitud serie
  | (temporadas serie) > 4 = 5
  | otherwise = (10 - (temporadas serie)) *2

bienestarReparto :: Serie -> Int
bienestarReparto serie
  | (length $ actores serie) < 10 = 3
  | otherwise = 10 - (tieneRestricciones 2 serie) 

bienestar :: Serie -> Int
bienestar serie
  | (cancelada serie) = 0
  | otherwise = (bienestarLongitud serie) + (bienestarReparto serie)

-- Punto 4

productorMasEfectivo :: [Serie] -> [Produccion] -> [Serie]
productorMasEfectivo series productores = map (masEfectivo productores) series

masEfectivo :: [Produccion] -> Serie -> Serie
masEfectivo (x:[]) serie = x serie 
masEfectivo (x:xs) serie
  | bienestar (x serie) > bienestar (head xs $ serie) = x serie
  | otherwise = masEfectivo xs serie

-- Punto 5

-- ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista infinita de actores?
-- si, se puede aplicar gatopardeitor con una lista infinita de actores. no se traba en consola.
-- como la funcion es la funcion id (identidad) devuelve infinitamente la serie que le paso, con la lista infinita de actores.
-- wl problema es que como tiene que mostrar una lista infinita de actores, nunca llego a ver los demas
-- atributos de la serie (temporadas, rating, etc).
-- si bien funciona en consola, no cumple con el proposito de la funcion.

serieEjemplo :: Serie
serieEjemplo = Serie "serie ejemplo" actoresInfinitos 100 2 5 False

actoresInfinitos = johnnyDepp : actoresInfinitos

-- > Resultados : 
-- Serie {nombre = "serie ejemplo", actores = [Actor {nombreActor = "johnny depp", sueldo = 20000000, restricciones = []},Actor {nombreActor = "johnny depp", sueldo = 20000000, restricciones = []} ....

-- ¿Y a uno con favoritismos? ¿De qué depende?

-- al aplicar conFavoritismo no hay problema al hacer el drop de los primero 2 elementos.
-- cuando se quiere agregar los favoritos a la lista puede ocurrir el problema: si se agregan al principio de la lista
-- no hay problema alguno, pero sí lo hay si se agregan al final de la lista (ya que nunca encontrara el final
-- de una lista infinita). 
-- por lo que depende de si agregamos a los actores al principio o al final

-- Punto 6

esControvertida :: Serie -> Bool
esControvertida serie = not $ cobraMasQueElSiguiente (actores serie)

cobraMasQueElSiguiente :: [Actor] -> Bool
cobraMasQueElSiguiente (x:[]) = True
cobraMasQueElSiguiente (x:xs) = (sueldo x) > (sueldo $ head xs) 

-- Punto 7

-- funcionLoca x y = filter (even.x) . map (length.y)

-- primero sabemos que hay dos parametro : x e y
-- como la primer funcion que se va a aplicar es map, sabemos que hay un tercer parametro implicito: z
-- z es una lista, no sabemos de que
-- funcionLoca :: -> -> [a] -> 
-- como y recibe la lista de z, debe tener su mismo tipo, pero puede devolver algo de otro tipo. lo unico que 
-- sabemos de este algo es que debe ser una lista, pues luego se le aplica la funcion length
-- funcionLoca :: -> (a -> [b]) -> [a] -> 
-- luego, se aplica filter. sabemos que el map devuelve una lista de Int y que sobre esa lista se aplicara el filter.
-- por lo que x es una funcion que recibe Int y devuelve un Int (ya que luego se le aplica even)
-- finalmente la funcion funcionLoca devuelve una lista de Int:
-- funcionLoca :: (Int -> Int) -> (a -> [b]) -> [a] -> [Int]