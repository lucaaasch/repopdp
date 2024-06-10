import PdePreludat
data Investigador = Investigador{
    nombre :: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving(Show,Eq)

data Item = Item{
    nombreItem :: String,
    valor :: Number
} deriving(Show,Eq)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

-- Punto 1

enloquecer :: Number -> Investigador -> Investigador
enloquecer n investigador = investigador {cordura = max 0 (cordura investigador - n)}

cambiarItems :: ([Item] -> [Item]) -> Investigador -> Investigador
cambiarItems funcion investigador = investigador {items = funcion (items investigador)}

-- Agregar un elemento al final de una lista
add :: [a] -> a -> [a]
add xs x = xs ++ [x]

hallarItem :: Item -> Investigador -> Investigador
hallarItem item = enloquecer (valor item) . cambiarItems (`add` item) 

-- Punto 2

-- NO HABIA LEIDO QUE DECIA NOMBRE DE ITEM, NO PIDE ESTO:

tieneItem1 :: Item -> [Investigador] -> Bool
tieneItem1 item  = any (== item) . concatMap items

tieneItem1' :: Item -> [Investigador] -> Bool
tieneItem1' item = any (elem item . items)

-- Ahora si, con nombre de item

tieneItem :: String -> [Investigador] -> Bool
tieneItem nom = any (== nom) . map (nombreItem) . concatMap items

tieneItem' :: String -> [Investigador] -> Bool
tieneItem' nom = any (any ((== nom) . nombreItem) . items)

-- Punto 3

potencial :: Investigador -> Number
potencial Investigador { cordura = 0 } = 0
potencial inv = 1 + 3 * length (sucesosEvitados inv) + length (items inv)

liderActual :: [Investigador] -> Investigador
liderActual = maximoSegun potencial

-- Punto 4

deltaSegun :: (b -> Number) -> (b -> b) -> b -> Number
deltaSegun ponderacion transformacion valor = abs ((ponderacion . transformacion) valor - ponderacion valor)

-- Punto 4.a

unDeltaCordura :: Number -> Investigador -> Number
unDeltaCordura n = deltaSegun cordura (enloquecer n) 

deltaCordura ::  Number -> [Investigador] -> Number
deltaCordura num = sum . map (unDeltaCordura num)

-- Punto 4.b

estaLoco :: Investigador -> Bool
estaLoco inv = cordura inv == 0

unDeltaPotencial :: [Investigador] -> Number
unDeltaPotencial = potencial . head . filter estaLoco

-- Punto 4.c

{-

    En el caso del deltaCordura no, pues se necesita conocer el unDeltaCordura de cada uno de los investigadores para luego hacer el sum, por ende, no hay resultado.
    En el caso de unDeltaPotencial pasa lo mismo, se esta haciendo un filter de "estaLoco", hasta que no se verifiquen todos los investigadores no tenemos una lista a la que aplicarle a head y asi..

-}

-- Punto 5

data Suceso = Suceso{
    descripcion :: String,
    consecuencias :: [Consecuencia],
    formaDeEvitar :: [Investigador] -> Bool
} deriving (Show,Eq)

type Consecuencia = [Investigador] -> [Investigador]

despertar :: Suceso
despertar = Suceso {descripcion = "Despertar de un antiguo", consecuencias = [map (enloquecer 10) , drop 1], formaDeEvitar = tieneItem "Necronomicon"}

dagaMaldita :: Item
dagaMaldita = Item {nombreItem = "Daga Maldita", valor = 3}

applyFirst :: (a -> a) -> [a] -> [a]
applyFirst _ []     = []
applyFirst f (x:xs) = f x : xs

ritual :: Suceso
ritual = Suceso {descripcion = "Ritual en Innsmouth", consecuencias = [enfrentarSuceso despertar, map (enloquecer 2), applyFirst (hallarItem dagaMaldita)], formaDeEvitar = (> 100) . potencial . liderActual}

-- Punto 6

sufrirConsecuencias :: [Consecuencia] -> Consecuencia
sufrirConsecuencias = foldr (.) id

agregarSuceso :: String -> Investigador -> Investigador
agregarSuceso suceso investigador = investigador {sucesosEvitados = sucesosEvitados investigador ++ [suceso]}

enfrentarSuceso :: Suceso -> [Investigador] -> [Investigador]
enfrentarSuceso suceso grupo
    | formaDeEvitar suceso grupo = map (enloquecer 1) grupo
    | otherwise = sufrirConsecuencias (consecuencias suceso) . map (enloquecer 1) $ grupo

-- Punto 7

deltaCorduraTotal :: Suceso -> [Investigador] -> Number
deltaCorduraTotal suceso grupo = sum (map cordura grupo) - sum (map cordura (enfrentarSuceso suceso grupo))

sucesoMasAterrador :: [Suceso] -> [Investigador] -> Suceso
sucesoMasAterrador sucesos grupo = maximoSegun (`deltaCorduraTotal` grupo) sucesos