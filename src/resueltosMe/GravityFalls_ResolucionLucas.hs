import PdePreludat

data Persona = Persona{
    edad :: Number,
    items :: [String],
    experiencia :: Number
}deriving (Show,Eq)

data Criatura = Criatura{
    peligrosidad :: Number,
    condicion :: Persona -> Bool
}deriving (Show,Eq)

nadaQueHacer :: Persona -> Bool
nadaQueHacer persona = False

siempredetras :: Criatura
siempredetras = Criatura {peligrosidad = 0, condicion = nadaQueHacer}

tieneSoplador :: Persona -> Bool
tieneSoplador = elem "Soplador de hojas" . items

gnomos :: Number -> Criatura
gnomos cantidad = Criatura {peligrosidad = 2 ^ cantidad, condicion = tieneSoplador}

fantasma :: Number -> (Persona -> Bool) -> Criatura
fantasma categoria asunto = Criatura {peligrosidad = categoria * 20, condicion = asunto}

modificarExperiencia :: (Number -> Number) -> Persona -> Persona
modificarExperiencia cambio persona = persona {experiencia = cambio (experiencia persona)}

enfrentarCriatura :: Criatura -> Persona -> Persona
enfrentarCriatura criatura persona
    | condicion criatura persona = modificarExperiencia ((+) . peligrosidad $ criatura) persona
    | otherwise = modificarExperiencia (+1) persona

experienciaGanada :: Persona -> [Criatura]  -> Number
experienciaGanada persona = experiencia . foldr enfrentarCriatura persona

requisitoFantasmaCategoria3 :: Persona -> Bool
requisitoFantasmaCategoria3 persona = edad persona < 13 && elem "Disfraz de oveja" (items persona)

requisitoFantasmaCategoria1 :: Persona -> Bool
requisitoFantasmaCategoria1 persona = experiencia persona > 10

aguluc :: Persona
aguluc = Persona 19 ["Disfraz de oveja","RTX 4090ti"] 100

inexperimentado :: Persona
inexperimentado = Persona 1 [] 0

criaturas3B :: [Criatura]
criaturas3B = [siempredetras,gnomos 10, fantasma 3 requisitoFantasmaCategoria3, fantasma 1 requisitoFantasmaCategoria1]

{-
    El ejemplo de consulta seria:
    > experienciaGanada aguluc criaturas3B
    123

    > experienciaGanada inexperimentado criaturas3B
    4 -- El resultado de 4 malas
-}

-- Parte 2

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] xB = xB
zipWithIf _ _ xA [] = []
zipWithIf transformacion condicion (xA : xsA) (xB : xsB)
    | not $ condicion xB = xB : zipWithIf transformacion condicion (xA : xsA) xsB
    | otherwise = transformacion xA xB : zipWithIf transformacion condicion xsA xsB

abecedario :: String
abecedario = "abcdefghijklmnopqrstuvwxyz"

posicionEnAbecedario :: Char -> Number
posicionEnAbecedario letra =  (+1) . length . takeWhile (/= letra) $ abecedario

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = drop (posicionEnAbecedario letra - 1) abecedario ++ take (posicionEnAbecedario letra - 1) abecedario

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraADescifrar = abecedarioDesde letraClave !! posicionEnAbecedario letraADescifrar

esLetra :: Char -> Bool
esLetra = flip elem abecedario

repetirNVeces :: Number -> [a] -> [a]
repetirNVeces n = concatMap (replicate n)

-- No entendi estos dos puntos la verdad
cesar :: Char -> String -> String
cesar letraClave = zipWithIf desencriptarLetra esLetra [letraClave] 

vignere :: String -> String -> String
vignere clave = zipWithIf desencriptarLetra esLetra (repetirNVeces (length clave) clave)
