import PdePreludat

data Turista = Turista{
    estres :: Number,
    cansancio :: Number,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show,Eq)

modificarCansancio :: (Number -> Number) -> Turista -> Turista
modificarCansancio cambio turista = turista {cansancio = cambio (cansancio turista)}

modificarEstres :: (Number -> Number) -> Turista -> Turista
modificarEstres cambio turista = turista {cansancio = cambio (estres turista)}

irALaPlaya :: Turista -> Turista
irALaPlaya turista
    | viajaSolo turista = modificarCansancio (subtract 5) turista
    | otherwise = modificarEstres (subtract 1) turista

apreciarElemento :: String -> Turista -> Turista
apreciarElemento elemento = modificarEstres (subtract . length $ elemento)

modificarPresencia :: Bool -> Turista -> Turista
modificarPresencia valor turista = turista {viajaSolo = valor}

modificarIdiomas :: ([String] -> [String]) -> Turista -> Turista
modificarIdiomas cambio turista = turista {idiomas = cambio (idiomas turista)}

salirAHablar :: String -> Turista -> Turista
salirAHablar idioma = modificarPresencia True . modificarIdiomas (++ [idioma])

conseguirNumberensidad :: Number -> Number
conseguirNumberensidad = flip div 4 -- Por el tema de los Number no se va a truncar pero bueno, es lo que hay

caminar :: Number -> Turista -> Turista
caminar minutos = modificarCansancio ( (+) . conseguirNumberensidad $ minutos) . modificarEstres (subtract . conseguirNumberensidad $ minutos)

paseoEnBarco :: String -> Turista -> Turista
paseoEnBarco comoEstaLaMarea turista
    | comoEstaLaMarea == "Fuerte" = modificarEstres (+6) . modificarCansancio(+10) $ turista
    | comoEstaLaMarea == "Moderada" = turista
    | otherwise = salirAHablar "Aleman" . apreciarElemento "Mar" . caminar 10 $ turista

ana :: Turista
ana = Turista {estres = 21, cansancio = 0, viajaSolo = False, idiomas = ["Español"]}

beto :: Turista
beto = Turista {estres = 15, cansancio = 15, viajaSolo = True, idiomas = ["Aleman"]}

cathi :: Turista
cathi = Turista {estres = 15, cansancio = 15, viajaSolo = True, idiomas = ["Aleman","Catalán"]}

type Excursion = (Turista -> Turista)

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion = modificarEstres (*0.9) . excursion

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaSegunExcursion :: (Turista -> Number) -> Excursion -> Turista  -> Number
deltaSegunExcursion indice excursion turista = abs $ deltaSegun indice (excursion turista) turista

excursionEsEducativa :: Excursion -> Turista -> Bool
excursionEsEducativa excursion = (/= 0) . deltaSegunExcursion (length . idiomas) excursion 

loDesestresa :: Excursion -> Turista -> Bool
loDesestresa excursion = (>3) . deltaSegunExcursion estres excursion 

excursionesDesestresantes :: Turista -> Tour -> Tour
excursionesDesestresantes turista = filter (`loDesestresa` turista)

type Tour = [Excursion]

completo :: Tour
completo = [salirAHablar "Melmacquiano", caminar 40 , apreciarElemento "Cascada" , caminar 20]

ladoB :: Excursion -> Tour
ladoB excursionElegida = [caminar 120 , paseoEnBarco "Aguas Tranquilas", excursionElegida]

excursionIslaVecina :: String -> Excursion
excursionIslaVecina estadoMarea
    | estadoMarea == "Fuerte" = apreciarElemento "Lago"
    | otherwise = irALaPlaya

islaVecina :: String -> Tour
islaVecina estadoMarea = [paseoEnBarco estadoMarea, excursionIslaVecina estadoMarea, paseoEnBarco estadoMarea]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldr ($) (modificarEstres ((+) . length $ tour) turista) tour

esConvicente :: Turista -> [Tour] -> Bool
esConvicente turista = null . map (excursionesDesestresantes (modificarPresencia True turista)) 

efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum . map (espiritualidadAportada tour) -- . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Number
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Number
deltaRutina tour turista = deltaSegun nivelDeRutina (hacerTour tour turista) turista

nivelDeRutina :: Turista -> Number
nivelDeRutina turista = cansancio turista + estres turista

-- Punto 4

playasEternas :: Tour
playasEternas = repeat irALaPlaya

-- b)
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c)
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}

