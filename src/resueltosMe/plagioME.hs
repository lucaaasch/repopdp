import PdePreludat

data Obra = Obra{
    texto :: String,
    año :: Number
} deriving (Show,Eq)

data Autor = Autor{
    nombre :: String,
    obras :: [Obra]
} deriving (Show,Eq)

-- Punto 1

obraA :: Obra
obraA = obraA {texto = "Había una vez un pato.", año = 1997}

obraB :: Obra
obraB = obraB {texto = "¡Había una vez un pato!", año = 1996}

obraC :: Obra
obraC = obraC {texto = "Mirtha, Susana y Moria.", año = 2010}

obraD :: Obra
obraD = obraC {texto = "La semántica funcional del amoblamiento vertebral es riboficiente", año = 2020}

obraE :: Obra
obraE = obraE {texto = "La semántica funcional de Mirtha, Susana y Moria.", año = 2022}

-- Punto 2

esCaracter :: Char -> Bool
esCaracter caracter = caracter == '!' || caracter == '¡' || caracter == '.'  || caracter == ',' 

textoCrudo :: String -> String
textoCrudo [] = []
textoCrudo (caracter : texto)
    | esCaracter caracter = textoCrudo texto
    | otherwise = caracter : textoCrudo texto

-- Punto 3

empiezaIgual :: Obra -> Obra -> Bool
empiezaIgual obra1 obra2 = texto obra1 == principio
    where principio = take (length (texto obra1)) (texto obra2)

agregaronIntro :: Obra -> Obra -> Bool
agregaronIntro obra1 obra2 = texto obra1 == final
    where final = drop (length (texto obra2) - length (texto obra1)) (texto obra2)

-- Hay que hacerla con lambda, pero la dejo aca
estaInvertido :: Obra -> Obra -> Bool
estaInvertido obra1 obra2 = reverse (texto obra1) == texto obra2

copiaLiteral :: Obra -> Obra -> Bool
copiaLiteral obra1 obra2 = texto obra1 == texto obra2

-- Si obra1 es plagio de obra2
esPlagio :: Obra -> Obra -> Bool
esPlagio obra1 obra2 = sePublicoDespues && algunCasoPlagio
    where algunCasoPlagio = copiaLiteral obra1 obra2 || empiezaIgual obra1 obra2 || agregaronIntro obra1 obra2 || (\ tx1 tx2 -> reverse tx1 == tx2) (texto obra1) (texto obra2) -- estaInvertido obra1 obra2
          sePublicoDespues = año obra1 > año obra2

-- Punto 4

data Bot = Bot{
    fabricante :: String,
    formasDeteccion :: Obra -> Obra -> Bool
} deriving (Show,Eq)

unBot :: Bot
unBot = Bot {fabricante = "Lucas", formasDeteccion = esPlagio}

otroBot :: Bot
otroBot = Bot {fabricante = "Mumuki", formasDeteccion = esPlagio}

-- Punto 5

botDeteccion :: Bot -> Obra -> Obra ->  Bool
botDeteccion = formasDeteccion

-- Punto 6

iteracionObras :: [Obra] -> [Obra] -> (Obra -> Obra -> Bool) -> Bool
iteracionObras [] [] _ = True
iteracionObras (obra1 : resto) (obra2 : resto2) deteccion = deteccion obra1 obra2 && iteracionObras resto resto2 deteccion

iteracionAutores :: [Autor] -> (Obra -> Obra -> Bool) -> Bool
iteracionAutores [] _ = True
iteracionAutores (autor1 : autor2 : resto) deteccion = iteracionObras (obras autor1) (obras autor2) deteccion && iteracionAutores resto deteccion

esCadenaPlagio :: [Autor] -> Bot -> Bool
esCadenaPlagio autores bot = iteracionAutores autores (formasDeteccion bot)

-- Punto 7
-- aprendieron bot autores = undefined -- No la quiero hacer XD

-- Punto 8

obraInfinita :: Obra
obraInfinita = Obra {texto = repeat 'a', año = 2021}

obraInfinita2 :: Obra
obraInfinita2 = Obra {texto = repeat 'a', año = 2025}

-- Punto 9

{-

Suponiendo una consulta como: deteccion obraA obraInfinita copiaLiteral
No importa cuál sea la forma de detección, como la fecha de la obra que se pregunta si es plagio es anterior, no se cumple y corta diciendo False, por Lazy evaluation no es necesario seguir evaluando el otro lado del &&.

Ahora veamos los casos donde se cumple que la fecha es posterior:

copiaLiteral:
- Suponiendo la consulta: deteccion obraInfinita obraA copiaLiteral
da False, por Lazy Evaluation. Al evaluar la igualdad de contenidos no necesita evaluar toda la lista infinita para saber que los strings son distintos, con los primeros caracteres alcanza.
- Pero si consulto deteccion obraInfinita2 obraInfinita copiaLiteral, se cuelga, porque para saber si dos strings iguales son iguales necesita recorrerlos todos, aún con lazy evaluation.

empiezaIgual:
- Suponiendo la consulta: deteccion obraInfinita obraA empiezaIgual
Entonces da False, pues verifica con take que sean iguales los contenidos y eso da false. Por Lazy evaluation no es necesario evaluar la lista infinita para el take.
- Suponiendo la consulta: deteccion obraInfinita2 obraInfinita empiezaIgual
Ahí se cuelga, porque nunca llega a comparar las longitudes de los contenidos, aún con lazy evaluation. Es decir, aún si una es infinita y la otra empieza igual, jamás cortará.

leAgregaronIntro:
- Suponiendo la consulta: deteccion obraInfinita obraA leAgregaronIntro
Aún teniendo lazy evaluation, para el calcular el length del contenido de la obra infinita se cuelga, antes de poder hacer el drop.
- Ahora, si hacemos al revés: deteccion obraA obraInfinita leAgregaronIntro
Se colgaría, pues se pide hacer un ultimosElementos, que a su vez necesita el length de la lista infinita, no hay Lazy evaluation que lo salve.
-}