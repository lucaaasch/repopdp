import PdePreludat

data Elemento = Elemento { 
    tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje-> Personaje)
} deriving (Show,Eq)

data Personaje = Personaje{
    nombre :: String,
    salud :: Number,
    elementos :: [Elemento],
    año :: Number
}deriving (Show,Eq)

-- Punto 1

mandarAlAño :: Number -> Personaje -> Personaje
mandarAlAño nuevoAño personaje = personaje { año = nuevoAño}

modificarSalud :: (Number -> Number) -> Personaje -> Personaje
modificarSalud cambio personaje = personaje { salud = cambio (salud personaje)}

meditar ::  Personaje -> Personaje
meditar = modificarSalud (*1.5)

causarDaño :: Number -> Personaje -> Personaje
causarDaño daño = modificarSalud (max 0 . subtract daño)

-- Punto 2

esMalvado :: Personaje -> Bool
esMalvado = elem "Maldad" . map tipo . elementos

dañoQueProduce :: Personaje -> Elemento -> Number
dañoQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

algunElementoLoMata :: Personaje -> Elemento -> Bool
algunElementoLoMata personaje elementos = (== 0) . salud . ataque elementos $ personaje

loMata :: Personaje -> Personaje -> Bool
loMata personaje enemigo = any (algunElementoLoMata personaje) $ elementos enemigo

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje = filter (loMata personaje)

asignarTipo :: String -> Elemento -> Elemento
asignarTipo nuevoTipo eleme = eleme {tipo = nuevoTipo}

asignarFuncionAtaque :: (Personaje-> Personaje) -> Elemento -> Elemento
asignarFuncionAtaque funcion eleme = eleme {ataque = funcion}

asignarFuncionDefensa :: (Personaje-> Personaje) -> Elemento -> Elemento
asignarFuncionDefensa funcion eleme = eleme {defensa = funcion}

-- Punto 3

aplanarDefensas :: [(Personaje -> Personaje)] -> (Personaje -> Personaje)
aplanarDefensas = foldr (.) id

aplicarMeditaciones :: Number -> Elemento -> Elemento
aplicarMeditaciones nivel = asignarFuncionDefensa . aplanarDefensas . replicate nivel $ meditar

concentracion :: Number -> Elemento -> Elemento
concentracion nivel = asignarTipo "Magia" . aplicarMeditaciones nivel

bajarUnPunto :: Personaje -> Personaje
bajarUnPunto = modificarSalud (subtract 10)

esbirros :: Elemento -> Elemento
esbirros = asignarTipo "Maldad" . asignarFuncionAtaque bajarUnPunto 

esbirrosMalvados :: Number -> [Elemento -> Elemento]
esbirrosMalvados  = flip replicate esbirros
