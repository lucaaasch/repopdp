-- Esta no es la resolucion oficial de la catedra, es mia (y puede tener errores)
import PdePreludat

data Persona = Persona{
    nombre :: String,
    recuerdos :: [String]
} deriving (Show,Eq)

suki :: Persona
suki = Persona {nombre = "Susana Kitimporta", recuerdos = ["abuelita","escuela primaria","examen aprobado","novio"]}

reemplazarEn :: Number -> String -> [String] -> [String]
reemplazarEn posicion nuevoElemento lista = take posicion lista ++ [nuevoElemento] ++ drop (posicion + 1) lista

reemplazar' :: Number -> String -> Persona -> Persona
reemplazar' posicion nuevoElemento persona = persona {recuerdos = reemplazarEn posicion nuevoElemento (recuerdos persona)}

reemplazar :: Number -> String -> Persona -> Persona
reemplazar posicion nuevoElemento = modificarRecuerdos (reemplazarEn posicion nuevoElemento)

modificarRecuerdos :: ([String] -> [String]) -> Persona -> Persona
modificarRecuerdos modificador persona = persona {recuerdos = modificador (recuerdos persona)}

mover :: Number -> Number -> Persona -> Persona
mover posicionUno posicionDos persona = reemplazar posicionDos (flip (!!) posicionUno $ recuerdos persona) . reemplazar posicionUno (flip (!!) posicionDos $ recuerdos persona) $ persona

quitar :: String -> Persona -> Persona
quitar recuerdoAQuitar = modificarRecuerdos (filter (/= recuerdoAQuitar))

invertir :: Persona -> Persona
invertir = modificarRecuerdos reverse

nop :: Persona -> Persona
nop persona = persona

type Pesadilla = (Persona -> Persona)

listaPesadillas :: [Pesadilla]
listaPesadillas = [mover 1 3, reemplazar 2 "examen desaprobado",quitar "novio",invertir]

componerPesadillas :: [Pesadilla] -> Pesadilla
componerPesadillas = foldr (.) id

dormir :: [Pesadilla] -> Persona -> Persona
dormir = componerPesadillas

recuerdoExtraño :: Pesadilla
recuerdoExtraño persona = persona {recuerdos = (\ recu -> take 3 . drop 1 . (++["Esto ES UN SUEÑO!!"]) . drop (length recu - 4) $ recu) $ recuerdos persona}

-- Parte 2: Situaciones excepcionales

segmentationFault :: [Pesadilla] -> Persona -> Bool
segmentationFault pesadillas persona = length pesadillas > length (recuerdos persona)

bugInicial :: [Pesadilla] -> Persona -> Bool
bugInicial pesadillas persona = persona /= head pesadillas persona

falsaAlarma :: [Pesadilla] -> Persona -> Bool
falsaAlarma pesadillas persona = dormir pesadillas persona == persona

cuantasSufrenSituacion :: ([Pesadilla] -> Persona -> Bool) -> [Pesadilla] -> [Persona] -> Number
cuantasSufrenSituacion situacion pesadillas = length . filter (situacion pesadillas)

seDetectaEnTodos :: ([Pesadilla] -> Persona -> Bool) -> [Pesadilla] -> [Persona] -> Bool
seDetectaEnTodos situacion pesadillas = all (situacion pesadillas)

-- Parte 3:

{-
    Ninguna de las dos se puede hacer. BugInicial evalua la persona entera (con su lista) y falsaAlarma lo mismo.
-}