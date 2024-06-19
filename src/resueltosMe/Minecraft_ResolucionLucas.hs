import PdePreludat
data Personaje = Personaje {
nombre:: String,
puntaje:: Number,
inventario:: [Material]
} deriving (Show,Eq)

type Material = String
data Receta = Receta{
    tiempo :: Number,
    materiales :: [Material],
    objetoRes :: Material
} deriving (Show,Eq)

-- LIBRERIA DE FUNCIONES LUCAS ---------------------------------------------------

-- Agregar un elemento al final de una lista
add :: [a] -> a -> [a]
add xs x = xs ++ [x]

-- Agregar la primera ocurrencia de un elemento en una lista (recursivamente)
-- Es la alternativa a delete

eliminarPrimeraOcurrencia :: Eq a => a -> [a] -> [a]
eliminarPrimeraOcurrencia _ [] = []
eliminarPrimeraOcurrencia x (y:ys)
    | x == y    = ys
    | otherwise = y : eliminarPrimeraOcurrencia x ys

-- Ver si una lista contiene a otra sublista
contieneSublista :: (Eq a) => [a] -> [a] -> Bool
contieneSublista sublista lista = all (`elem` lista) sublista

-- Eliminar todos los elementos de una sublista en una lista
eliminarListaDentroDeOtra :: Eq a => [a] -> [a] -> [a]
eliminarListaDentroDeOtra [] listaOriginal = listaOriginal
eliminarListaDentroDeOtra xs listaOriginal = foldl (flip eliminarPrimeraOcurrencia) listaOriginal xs

-----------------------------------------------------------------------------------

-----------------
-- PARTE 1: Craft
-----------------

-- Punto 1

mesa :: Receta
mesa = Receta {tiempo = 5, materiales = ["Madera","Palo"], objetoRes = "Mesa"}

agregarObjeto :: Material -> Personaje -> Personaje
agregarObjeto objeto personaje = personaje {inventario = add (inventario personaje) objeto}

eliminarMaterial :: Material -> Personaje -> Personaje
eliminarMaterial material personaje = personaje {inventario = eliminarPrimeraOcurrencia material (inventario personaje)}

cambiarPuntos :: (Number -> Number) -> Personaje -> Personaje
cambiarPuntos func personaje = personaje {puntaje = func (puntaje personaje)}

incrementarPuntos :: Receta -> Personaje -> Personaje
incrementarPuntos receta = cambiarPuntos (+ 10 * tiempo receta)

restarPuntos :: Number -> Personaje -> Personaje
restarPuntos n = cambiarPuntos (subtract n)

tieneMateriales :: Receta -> Personaje -> Bool
tieneMateriales receta personaje = contieneSublista (materiales receta) (inventario personaje)

gastarMateriales :: Receta -> Personaje -> Personaje
gastarMateriales receta personaje = personaje {inventario = eliminarListaDentroDeOtra (materiales receta) (inventario personaje)} 

craftear :: Receta -> Personaje -> Personaje
craftear receta personaje
    | tieneMateriales receta personaje = incrementarPuntos receta . agregarObjeto (objetoRes receta) . gastarMateriales receta $ personaje
    | otherwise = restarPuntos 100 personaje

-- Punto 2

duplicaPuntos :: Personaje -> Receta -> Bool
duplicaPuntos personaje receta = puntaje (craftear receta personaje) > 2 * puntaje personaje

puedeCraftear :: Personaje -> Receta -> Bool
puedeCraftear personaje receta = tieneMateriales receta personaje

crafteosQueDuplican :: Personaje -> [Receta] -> [Receta]
crafteosQueDuplican personaje = filter (duplicaPuntos personaje)  . filter (puedeCraftear personaje) 

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldr craftear

ordenEstablecidoEsMejor :: Personaje -> [Receta] -> Bool
ordenEstablecidoEsMejor personaje crafteos = puntaje ordenIndicado > puntaje ordenInverso 
    where ordenIndicado = craftearSucesivamente personaje crafteos
          ordenInverso = craftearSucesivamente personaje (reverse crafteos)

-----------------
-- PARTE 2: Mine
-----------------

-- Punto 1
data Bioma = Bioma{
    materialesBioma :: [Material],
    objetoNecesario :: Material
}deriving(Show,Eq)

type Mundo = [Bioma]

tieneMaterial :: Material -> Personaje -> Bool
tieneMaterial mat per = mat `elem` inventario per

type Herramienta = [Material] -> Material

minar :: Herramienta -> Bioma -> Personaje -> Personaje
minar herramienta bioma personaje
    | tieneMaterial (objetoNecesario bioma) personaje = cambiarPuntos (+50) . agregarObjeto (herramienta (materialesBioma bioma)) $ personaje
    | otherwise = personaje

-- Punto 2

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Number -> Herramienta
pico = flip (!!)

-- Consigue el elemento en la segunda posicion
hazada ::  Herramienta
hazada = flip (!!) 2

-- Consigue el elemento en la anteultima posicion
pala :: Herramienta
pala lista = (!!) lista (length lista - 1)

-- Punto 2.a

materialDelMedio :: Herramienta
materialDelMedio lista = (!!) lista . flip (/) 2 $ length lista

-- Punto 2.b

paLambda :: Bioma -> Bioma -> Herramienta
paLambda b1 b2 = flip (!!) $ (\ unBioma otroBioma -> length unBioma + length otroBioma) (materialesBioma b1) (objetoNecesario b2)

-- Punto 3

steve :: Personaje
steve = Personaje {nombre = "Steve", puntaje = 0, inventario =  ["Madera","Palo","Madera","Palo","Azucar"]}

nieve :: Bioma
nieve = Bioma {materialesBioma = cycle ["Bola de Nieve","Zanahoria"], objetoNecesario = "Sueter"}

{-
> minar espada nieve steve
Personaje {nombre = "Steve", puntaje = 0, inventario =  ["Madera","Palo","Madera","Palo","Azucar"]}

Como podemos ver, se ejecuta y se termina.

Esto se debe a que steve no tenia el objeto necesario para minar en la nieve
-}