import PdePreludat

data Serie = Serie{
    nombre :: String,
    actores :: [Actor],
    presupuesto :: Number,
    temporadas :: Number,
    rating :: Number,
    estaCancelada :: Bool
}deriving (Show, Eq)

data Actor = Actor{
    nombreActor :: String,
    sueldo :: Number,
    restricciones :: [String]
} deriving (Show, Eq)

loQueQuierenCobrar :: [Actor] -> Number
loQueQuierenCobrar = sum . map sueldo

estaEnRojo :: Serie -> Bool
estaEnRojo serie = presupuesto serie < loQueQuierenCobrar (actores serie)

filtroRestricciones :: [Actor] -> Bool
filtroRestricciones = null . filter (>1) . map (length . restricciones)

esProblematica :: Serie -> Bool
esProblematica serie = filtroRestricciones (actores serie)

johnnyDepp :: Actor
johnnyDepp = Actor {nombreActor = "Johnny Depp", sueldo = 20000000, restricciones = []}

helenaBonham :: Actor
helenaBonham = Actor {nombreActor = "Helena Bonham", sueldo = 15000000, restricciones = []}

type Productor = Serie -> Serie

cambiarActores :: ([Actor] -> [Actor]) -> Serie -> Serie
cambiarActores cambio serie = serie {actores = cambio (actores serie)}

conFavoritismos :: Actor -> Actor -> Productor
conFavoritismos actorUno actorDos = cambiarActores (flip (++) [actorDos] . flip (++) [actorUno] . drop 2)

timBurton :: Productor
timBurton = conFavoritismos johnnyDepp helenaBonham

gatopardeitor :: Productor
gatopardeitor serie = serie

cambiarTemporadas :: (Number -> Number) -> Serie -> Serie
cambiarTemporadas cambio serie = serie {temporadas = cambio (temporadas serie)}

estireitor :: Productor
estireitor = cambiarTemporadas (*2)

desespereitor :: Productor
desespereitor = timBurton . estireitor

cancelarSerie :: Serie -> Serie
cancelarSerie serie = serie {estaCancelada = True}

canceleitor :: Number -> Productor
canceleitor cifra serie
    | estaEnRojo serie || rating serie < cifra = cancelarSerie serie
    | otherwise = serie

bienestarTemporadas :: Serie -> Number
bienestarTemporadas serie
    | temporadas serie > 4 = 5
    | otherwise = (10 - temporadas serie) * 2

cantActoresRestricciones :: [Actor] -> Number
cantActoresRestricciones = length . filter (>1) . map (length . restricciones)

bienestarActores :: Serie -> Number
bienestarActores serie
    | length (actores serie) < 10 = 3
    | otherwise = 10 - max 2 (cantActoresRestricciones (actores serie))

calcularBienestar :: Serie -> Number
calcularBienestar serie
    | estaCancelada serie = 0
    | otherwise = bienestarActores serie + bienestarTemporadas serie

productorMasEfectivo :: [Productor] -> Serie -> Productor
productorMasEfectivo [prod] serie = prod
productorMasEfectivo (prod1 : prod2 : resto) serie
    | calcularBienestar (prod1 serie) > calcularBienestar (prod2 serie) = productorMasEfectivo (prod1 : resto) serie
    | otherwise = productorMasEfectivo (prod2 : resto) serie

conseguirProductoresMasEfectivos ::  [Productor] -> [Serie] -> [Productor]
conseguirProductoresMasEfectivos listaProductores = map (productorMasEfectivo listaProductores) 

aplicarProductoresEfectivos :: [Productor] -> [Serie] -> [Serie]
aplicarProductoresEfectivos listaProductores listaSeries = zipWith ($) (conseguirProductoresMasEfectivos listaProductores listaSeries) listaSeries

-- Punto 5

{-
    Gatopardeitor se puede aplicar a una serie con una lista infinita de actores pues simplemente devuelve esa serie, por supuesto que el Show nunca termina pero poder se puede
    Favoritismos tambien, solo se encarga de dropear 2 y agregar 2 al principio
-}

-- Punto 6

todosCobranMasQueSuSiguiente :: [Actor] -> Bool
todosCobranMasQueSuSiguiente [_] = True
todosCobranMasQueSuSiguiente (actorUno : actorDos : resto) = sueldo actorUno > sueldo actorDos && todosCobranMasQueSuSiguiente (actorDos : resto)

esControvertida :: Serie -> Bool
esControvertida serie = todosCobranMasQueSuSiguiente (actores serie)

-- Punto 7

funcionLoca :: (Number -> Number) -> (a1 -> [a2]) -> [a1] -> [Number]
funcionLoca x y = filter (even . x) . map (length . y)

{-
    Funcion Loca tiene tres parametros, x (una funcion que modifica un numero), y (una funcion que agarra los elementos de z y los aplica en una lista de 'a2') y z (una lista de 'a1') retornando una lista de numeros
-}

