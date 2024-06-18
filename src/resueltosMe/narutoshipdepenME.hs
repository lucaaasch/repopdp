import PdePreludat

-- Parte A
data Ninja = Ninja{
    nombre :: String,
    herramientas :: [Herramienta],
    jutsusDelNinja :: [Jutsu],
    rango :: Number
}deriving (Show,Eq)

data Herramienta = Herramienta{
    nombreHerramienta :: String,
    cantidad :: Number
}deriving (Show,Eq)

modificarCantidad :: Number -> Herramienta -> Herramienta
modificarCantidad nuevaCantidad herramienta = herramienta {cantidad = nuevaCantidad}

herramientasTotales :: [Herramienta] -> Number
herramientasTotales = sum . map cantidad

obtenerHerramienta ::  Number -> Herramienta -> Ninja -> Ninja
obtenerHerramienta  cantidad herramienta ninja
    | herramientasTotales (herramientas ninja) + cantidad <= 100 = modificarHerramientas (++ [herramienta]) ninja
    | otherwise = modificarHerramientas (drop (herramientasTotales (herramientas ninja) - 100)) . modificarHerramientas (++ [herramienta]) $ ninja

modificarHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
modificarHerramientas cambio ninja = ninja {herramientas = cambio (herramientas ninja)}

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta herramientaAEliminar = modificarHerramientas (filter (/= herramientaAEliminar))

-- Parte B

data Mision = Mision{
    cantidadNinjas :: Number,
    rangoRecomendable :: Number,
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving (Show,Eq)

esDeMenorRango :: Mision -> Ninja -> Bool
esDeMenorRango mision ninja = rangoRecomendable mision > rango ninja

algunoMenorRango :: Mision -> [Ninja] -> Bool
algunoMenorRango mision = any (esDeMenorRango mision)

esDesafiante :: [Ninja] -> Mision -> Bool
esDesafiante  ninjas mision = length (enemigos mision) > 2 && algunoMenorRango mision ninjas

kunais :: Herramienta
kunais = Herramienta {nombreHerramienta = "Kunais", cantidad = 14}

shurikens :: Herramienta
shurikens = Herramienta {nombreHerramienta = "Shurikens", cantidad = 5}

bombasHumo :: Herramienta
bombasHumo = Herramienta {nombreHerramienta = "Bombas de Humo", cantidad = 3}

esCopada :: Mision -> Bool
esCopada mision = recompensa mision == kunais || recompensa mision == shurikens || recompensa mision == bombasHumo

herramientasTotalesEquipo :: [Ninja] -> Number
herramientasTotalesEquipo = herramientasTotales . concatMap herramientas

esFactible :: [Ninja] -> Mision -> Bool
esFactible ninjas mision = not (esDesafiante ninjas mision) && segundaCondicion
    where segundaCondicion = length ninjas == cantidadNinjas mision  || herramientasTotalesEquipo ninjas > 500

modificarRango :: (Number -> Number) -> Ninja -> Ninja
modificarRango cambio ninja = ninja {rango = cambio (rango ninja)}

tieneRangoRecomendado :: Mision -> Ninja -> Bool
tieneRangoRecomendado mision ninja = rango ninja >= rangoRecomendable mision

fallarMision :: Mision -> [Ninja] -> [Ninja]
fallarMision mision = map (modificarRango (max 0 . subtract 2)) . filter (not . esDeMenorRango mision)

cumplirMision :: Mision -> [Ninja] -> [Ninja]
cumplirMision mision = map (obtenerHerramienta cantidadHerramienta (recompensa mision) . modificarRango (+1))
    where cantidadHerramienta = cantidad (recompensa mision)

type Jutsu = Mision -> Mision

modificarNinjasNecesarios :: (Number -> Number) -> Mision -> Mision
modificarNinjasNecesarios cambio mision = mision {cantidadNinjas = cambio (cantidadNinjas mision)}

clonesDeSombra :: Number -> Jutsu
clonesDeSombra clones = modificarNinjasNecesarios (max 1 . subtract clones)

modificarEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
modificarEnemigos cambio mision = mision {enemigos = cambio (enemigos mision)}

rangoMenorA500 :: Ninja -> Bool
rangoMenorA500 ninja = rango ninja < 500

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = modificarEnemigos (filter (not . rangoMenorA500))

naruto :: Ninja
naruto = Ninja {nombre = "Naruto", herramientas = [], jutsusDelNinja = [clonesDeSombra 3, fuerzaDeUnCentenar], rango = 1000}

gokuOnDrugs :: Ninja
gokuOnDrugs = Ninja {nombre = "Goku On Drugs", herramientas = [], jutsusDelNinja = [clonesDeSombra 9, fuerzaDeUnCentenar], rango = 1000}

jutsusTotales :: [Ninja] -> Jutsu
jutsusTotales = foldr (.) id . concatMap jutsusDelNinja

aplicarJutsus :: [Ninja] -> Mision -> Mision
aplicarJutsus = jutsusTotales  

ejecutarMision :: Mision -> [Ninja] -> [Ninja]
ejecutarMision mision ninjas
    | copada || factible = cumplirMision mision ninjas
    | otherwise = fallarMision mision ninjas
        where copada = esCopada (aplicarJutsus ninjas mision)
              factible = esFactible ninjas (aplicarJutsus ninjas mision)

abanicoUchiha :: Herramienta
abanicoUchiha = Herramienta {nombreHerramienta = "Abanico de Madara Uchiha", cantidad = 1}

granGuerraNinja :: Mision
granGuerraNinja = Mision {cantidadNinjas = 100000, rangoRecomendable = 100, enemigos = [], recompensa = abanicoUchiha}

zetsu :: Ninja
zetsu = Ninja {nombre = "Zetsu", herramientas = [], jutsusDelNinja = [], rango = 600}

modificarNombre :: (String -> String) -> Ninja -> Ninja
modificarNombre cambio ninja = ninja {nombre = cambio (nombre ninja)}

enemigosGranGuerra :: [Ninja]
enemigosGranGuerra = [modificarNombre (++ show [1..]) zetsu]

{-
  Si la misión es copada, termina de ejecutar sin problemas y se cumple la misión.
  Si el equipo es finito y la misión no es desafiante porque el equipo no tiene un miembro no calificado, termina sin problemas y se cumple la misión.
  Si el equipo es finito, la misión no es desafiante porque el equipo no tiene un miembro no calificado, y no es factible porque el equipo no está bien preparado, termina sin problemas y se falla la misión.
  En caso contrario, no termina de evaluar, ya sea porque tiene que evaluar la totalidad de la lista de enemigos, o la totalidad del equipo.
-}