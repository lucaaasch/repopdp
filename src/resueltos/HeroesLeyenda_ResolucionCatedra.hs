import Text.Show.Functions
import Data.Char

-- Punto 1 ----------------------------------------

type Labor = [Tarea]
type Resultado = (Heroe, Heroe)
type Debilidad = Heroe -> Bool
type Tarea = Heroe -> Heroe
type Rareza = Int
type Artefacto = (String, Rareza)

data Heroe = Heroe {
  epiteto :: String,
  reconocimiento :: Int,
  artefactos :: [Artefacto],
  titulos :: [String],
  -- no es necesario usar una lista de titulos. Solo la usa 1 vez, ayudarACruzarLaCalle.
  tareas :: [Tarea]
} deriving (Show)

data Bestia = Bestia {
  nombre :: String,
  debilidad :: Debilidad
} deriving (Show)

-- Mapeos

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto unaFuncion heroe = heroe { epiteto = unaFuncion (epiteto heroe) }

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto unaFuncion heroe = heroe { artefactos = unaFuncion (artefactos heroe) }

mapReconocimiento :: (Int -> Int) -> Heroe -> Heroe
mapReconocimiento unaFuncion heroe = heroe { reconocimiento = unaFuncion (reconocimiento heroe) }

mapRarezaDelArtefacto :: (Rareza -> Rareza) -> Artefacto -> Artefacto
mapRarezaDelArtefacto unaFuncion (nombre, rareza) = (nombre, unaFuncion rareza)

mapTarea :: ([Tarea] -> [Tarea]) -> Heroe -> Heroe
mapTarea unaFuncion heroe = heroe { tareas = unaFuncion (tareas heroe) }

mapTitulo :: ([String] -> [String]) -> Heroe -> Heroe
mapTitulo unaFuncion heroe = heroe { titulos = unaFuncion (titulos heroe) }


-- Punto 2 ----------------------------------------

paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe
  | reconocimiento unHeroe > 1000 = cambiarEpitetoPor "El mitico" unHeroe
  | reconocimiento unHeroe >= 500 = aniadirArtefacto ("Lanza Del Olimpo", 100) $ cambiarEpitetoPor "El magnifico" unHeroe
  | estaEntre 500 100 (reconocimiento unHeroe) = aniadirArtefacto ("Xiphos", 50) $ cambiarEpitetoPor "Hoplita" unHeroe
  -- complicacion de mas, se podria haber hecho lo mismo que hizo arriba, >100
  -- podria haber abstraido los objetos
  | otherwise = unHeroe

cambiarEpitetoPor nuevoEpiteto = mapEpiteto (const nuevoEpiteto)

aniadirArtefacto nuevoArtefacto = mapArtefacto (nuevoArtefacto :)

estaEntre extremo1 extremo2 valor = (extremo1 > valor && extremo2 < valor) || (extremo2 > valor && extremo1 < valor)


-- Punto 3 ----------------------------------------

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto (nombre, rareza) = aniadirArtefacto (nombre, rareza) . ganarReconocimiento rareza

escalarElOlimpo :: Tarea
escalarElOlimpo = aniadirArtefacto ("El relampago de Zeus", 500) . desecharAlgunosArtefactos . triplicarRarezaDeArtefactos . ganarReconocimiento 500

--ayudarACruzarLaCalle :: Int -> Tarea
--ayudarACruzarLaCalle cuadras = obtenerTitulo ("Gros" ++ (take cuadras listaDeOsInfinitas))

-- la lista de titulos nunca fue necesaria, era el epíteto.

matarAUnaBestia :: Bestia -> Tarea
matarAUnaBestia unaBestia unHeroe
  | (debilidad unaBestia) unHeroe = cambiarEpitetoPor ("El asesino de " ++ (nombre unaBestia)) unHeroe
  | otherwise = pierdePrimerArtefacto $ cambiarEpitetoPor ("El cobarde") unHeroe

agregarTarea nuevaTarea = mapTarea (nuevaTarea :)

ganarReconocimiento unValor = mapReconocimiento (+ unValor)

triplicarRarezaDeArtefactos = mapArtefacto (map (mapRarezaDelArtefacto (*3)))

desecharAlgunosArtefactos = mapArtefacto (filter ( (> 1000) . snd))

obtenerTitulo nuevoTitulo = mapTitulo (nuevoTitulo :)

listaDeOsInfinitas = 'o' : listaDeOsInfinitas

pierdePrimerArtefacto = mapArtefacto (drop 1)


-- Punto 4 ----------------------------------------

heracles = Heroe "Guardian del Olimpo" 700 [("pistola", 1000), ("El relampago de Zeus", 500)] [] [matarAlLeonDeNemea]

-- Hubiese abstraido los artefactos.
-- El relampago de zeus se repite


-- Punto 5 ----------------------------------------

leonDeNemea = Bestia "Leon de Nemea" epitetoMayorA20

epitetoMayorA20 :: Debilidad
epitetoMayorA20 = (> 20) . length . epiteto

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarAUnaBestia leonDeNemea


-- Punto 6 ----------------------------------------

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea = agregarTarea unaTarea . unaTarea


-- Punto 7 ----------------------------------------

presumirLogros :: Heroe -> Heroe -> Resultado
presumirLogros unHeroe otroHeroe
  | reconocimiento unHeroe > reconocimiento otroHeroe = (unHeroe, otroHeroe)
  | reconocimiento unHeroe < reconocimiento otroHeroe = (otroHeroe, unHeroe)
  | otherwise = cuandoSonIguales unHeroe otroHeroe

cuandoSonIguales unHeroe otroHeroe
  | sumatoriaRarezaArtefactos unHeroe > sumatoriaRarezaArtefactos otroHeroe = (unHeroe, otroHeroe)
  | sumatoriaRarezaArtefactos unHeroe < sumatoriaRarezaArtefactos otroHeroe = (otroHeroe, unHeroe)
  | otherwise = presumirLogros (realizarTareasDelOtro unHeroe otroHeroe) (realizarTareasDelOtro otroHeroe unHeroe)

-- Se puede mejorar el hecho de haber usado 5 guardas en total

realizarTareasDelOtro unHeroe otroHeroe = realizarUnaLabor otroHeroe. tareas . realizarUnaLabor unHeroe $ tareas otroHeroe

sumatoriaRarezaArtefactos = sum . map snd . artefactos


-- Punto 8 ----------------------------------------
{-

¿Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, ningún artefacto y ninguna tarea realizada?

Primero, van a entrar a la guarda de "cuando son iguales". Como no tienen artefactos van a ir al otherwise. Cuando hago un foldr con listas vacias no rompe ya que tienen un elemento inicial de acumulacion, por lo que cada fold me va a devolver al mismo heroe. Por la llamada recursiva vuelve al comienzo y nunca termina de evaluar. Pruebas:

-}

heroe1 :: Heroe
heroe1 = Heroe "Heroe 1" 100 [] [] []

heroe2 :: Heroe
heroe2 = Heroe "Heore 2" 100 [] [] []

-- >> Resultados: se queda evaluando


-- Punto 9 ----------------------------------------

realizarUnaLabor unHeroe = foldr ($) unHeroe

-- Tiparla. No aprovecha hacerUnaTarea.


-- Punto 10 ----------------------------------------
{-
Si invocamos la función anterior con una labor infinita, ¿se podrá conocer el estado final del héroe? ¿Por qué?

No, dado que el fold seguira aplicando los elementos de la lista al valor resultante y nunca terminara de evaluar ya que siempre habra otro elemento que aplicarle.
Pruebas:
-}

listaDeTareasInfinitas = escalarElOlimpo : listaDeTareasInfinitas

heroe3 :: Heroe
heroe3 = Heroe "Heore 2" 100 [] [] listaDeTareasInfinitas

-- >> Resultados: realizarUnaLabor heroe3 listaDeTareasInfinitas Se queda evaluando indeterminadamente.

-- Conclusion: Las abstracciones que se pierde de hacer no las veo como determinantes. Aplica bien los conceptos del paradigma.
-- Bastante composicion. Se olvida de tipar una funcion importante. Teoricos bien respondidos.