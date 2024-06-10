import PdePreludat

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
  mail :: Mail,
  busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo :: [Depto]
deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo",
  Depto 1 45 3500 "Villa Urquiza",
  Depto 2 50 5000 "Palermo",
  Depto 1 45 5500 "Recoleta"
  ]

-- Punto 1.a 

comparar :: Ord b => (b -> b -> Bool) -> (a -> b) -> a -> a -> Bool
comparar criterio funcion valorUno valorDos = criterio (funcion valorUno) (funcion valorDos)

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor = comparar (>)

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor = comparar (<)

-- Punto 1.b

{- 

> ordenarSegun (mayor length) ["AA","ZZs","BCAA","CB"]
["AA","BC","CB","ZZ"]

-}

-- Punto 2.a

ubicadoEn :: Depto -> [Barrio] -> Bool
ubicadoEn = elem . barrio

-- Punto 2.b

cumpleRango :: (Depto -> Number) -> Number -> Number -> Depto -> Bool
cumpleRango funcion cotaInferior cotaSuperior depto = between cotaInferior cotaSuperior (funcion depto)

-- Punto 3.a

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all ($ depto)

-- Punto 3.b

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar requisitos criterio = ordenarSegun criterio . filter (flip cumpleBusqueda requisitos)

-- Punto 3.c

{- 

buscar ["Palermo", cumpleRango ambientes 1 2, cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo

-}

-- Punto 4

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = map mail . filter (any (cumpleBusqueda depto) . busquedas)

