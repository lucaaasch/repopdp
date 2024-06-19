import PdePreludat

type Sabor = String

-- Punto 1.a

data Postre = Postre{
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
}deriving(Show,Eq)

oreo :: Postre
oreo = Postre {sabores = ["Crema","Oreo","Chocolate"], peso = 80, temperatura = 10}

crema :: Postre
crema = Postre {sabores = ["Leche","Vainilla"], peso = 50, temperatura = 5}

coco :: Postre
coco = Postre {sabores = ["Coco","Crema","Leche","Maiz"], peso = 100, temperatura = 15}

listaPostres :: [Postre]
listaPostres = [oreo,crema,coco]

listaSabores :: [[Sabor]]
listaSabores = [sabores oreo, sabores crema, sabores coco]

-- Punto 1.b

type Hechizo = Postre -> Postre

cambiarTemperatura :: (Number -> Number) -> Hechizo
cambiarTemperatura cambio postre = postre {temperatura = cambio (temperatura postre)}

decrementarPeso :: Number -> Hechizo
decrementarPeso porcentaje postre = postre {peso = peso postre * (1 - porcentaje/100)}

cambiarSabores :: ([Sabor] -> [Sabor]) ->  Hechizo
cambiarSabores funcion postre = postre {sabores = funcion (sabores postre)}

agregarALista :: a -> [a] -> [a]
agregarALista lista nuevoElemento = lista : nuevoElemento

incendio :: Hechizo
incendio = cambiarTemperatura (+1) . decrementarPeso 5

immolobus :: Hechizo
immolobus postre = cambiarTemperatura (\ tempActual -> tempActual - temperatura postre) postre

wingardium :: Hechizo
wingardium = decrementarPeso 10 . cambiarSabores (agregarALista "Concentrado")

diffindo :: Number -> Hechizo
diffindo = decrementarPeso 

riddikulus :: Sabor -> Hechizo
riddikulus nuevoSabor = cambiarSabores (agregarALista (reverse nuevoSabor))

avadakedavra :: Hechizo
avadakedavra postre = cambiarSabores (drop cantSabores) postre
    where cantSabores = length (sabores postre)

-- Punto 1.c

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 &&  not (null (sabores postre)) && temperatura postre > 0

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo . hechizo)

-- Punto 1.d

pesoPromedioListos :: [Postre] -> Number
pesoPromedioListos = sum . map peso . filter estaListo

-- Punto 2

data Mago = Mago{
    hechizosAprendidos :: [Hechizo],
    horrorcruxes :: Number
} deriving (Show, Eq)

maguito :: Mago
maguito = Mago {hechizosAprendidos = [incendio,wingardium], horrorcruxes = 0}

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago {hechizosAprendidos = agregarALista hechizo (hechizosAprendidos mago)}

sumarUnHorrorcrux :: Mago -> Mago
sumarUnHorrorcrux mago = mago {horrorcruxes = horrorcruxes mago + 1}


conseguirSaboresHechizados :: Hechizo -> Postre -> [Sabor]
conseguirSaboresHechizados hechizo = sabores . hechizo

claseDefensa :: Hechizo -> Postre -> Mago -> Mago
claseDefensa hechizo postre
    | null (conseguirSaboresHechizados hechizo postre) = sumarUnHorrorcrux . agregarHechizo hechizo
    | otherwise = agregarHechizo hechizo

-- Punto 2.b

conseguirMejorHechizo :: Postre -> Mago -> Hechizo
conseguirMejorHechizo postre mago = foldl1 (esMejor postre) (hechizosAprendidos mago)

esMejor :: Postre -> Hechizo -> Hechizo -> Hechizo
esMejor postre h1 h2
    | length (conseguirSaboresHechizados h1 postre) > length (conseguirSaboresHechizados h2 postre) = h1
    | otherwise = h2

listaHechizos :: [Hechizo]
listaHechizos = [incendio,immolobus,wingardium, diffindo 10 , riddikulus "XD", avadakedavra]

magoInfinito :: Mago
magoInfinito = Mago { hechizosAprendidos = cycle listaHechizos, horrorcruxes = 0}

-- Punto 3.a

{- 
    Lista infinita de postres: > cycle listaPostres
    Mago con infinitos hechizos : > magoInfinito
-}

-- Punto 3.b

{-
Verdadero, existe la consulta:
Prelude> estanListos avadaKedabra mesaInfinita
La ejecución devuelve falso pues debido a la evaluación diferida, el all cuando encuentra el primer postre que no está listo ya retorna y no requiere construir la lista infinita.
-}

-- Punto 3.c 

{-
No existe ninguna forma de conocer el mejor hechizo del mago porque para hacerlo hay que evaluar todos los elementos lista, aún teniendo lazy evaluation.
-}