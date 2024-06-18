import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

cambiarElemento :: Number -> a -> [a] -> [a]
cambiarElemento posicion elemento lista = take (posicion - 1) lista ++ [ elemento ] ++ drop posicion lista

{-
1. Generar el modelo que represente los edificios y los departamentos. Si se encuentra alguna otra abstracción útil, incluirla.
-}

data Departamento = Departamento
 {
    superficie :: Number,
    habitabilidad :: Number
 }

data Piso = Piso 
 {
   numero :: Number,
   departamentos :: [Departamento]
 }

data Edificio = Edificio
 {
    pisos :: [Piso],
    valorPorMetro :: Number,
    robustez :: Number
 }

{-
2. Conocimientos de edificios:
a. Cheto: decimos que un edificio es cheto, cuando todos sus pisos tienen un único departamento.
b. Pajarera: Cuando los pisos tienen al menos 6 departamentos cada uno.
c. Pirámide: Cuando cada piso tiene menos departamentos que el piso inmediato inferior.
-}

departamentosDelPiso = length.departamentos

compararEdificiosPorDepartamentosDePisos criterio =  all (criterio.departamentosDelPiso)

esCheto = compararEdificiosPorDepartamentosDePisos (==1)

esPajarera = compararEdificiosPorDepartamentosDePisos (>=6)

tieneMenosDepartamentos :: Piso -> Piso -> Bool
tieneMenosDepartamentos pisoA pisoB =  ((<).departamentosDelPiso $ pisoB) . departamentosDelPiso $ pisoA

setearPisosAComparar :: [Piso] -> [(Piso,Piso)]
setearPisosAComparar pisos = zip pisos (drop 1 pisos)

esPiramide :: Edificio -> Bool
esPiramide edificio = foldr (\pisosAComparar semilla -> uncurry tieneMenosDepartamentos pisosAComparar && semilla ) True (setearPisosAComparar.pisos $ edificio)

{-
3. Conocer el precio del departamento más caro de un edificio, según su superficie y el valor base del metro cuadrado del edificio, multiplicado por el coeficiente de robustez del mismo.
-}

calcularPrecioBase :: Edificio -> Number
calcularPrecioBase edificio = (*(valorPorMetro edificio)).robustez $ edificio 

calcularPrecio :: Edificio -> Departamento -> Number
calcularPrecio edificio = (*(calcularPrecioBase edificio)).superficie

calcularYElegirMasCaro :: Edificio -> Number -> Departamento -> Number
calcularYElegirMasCaro edificio valor departamento = ((max valor).(calcularPrecio edificio)) departamento

calcularPrecioDepartamentoMasCaro :: Edificio -> Number
calcularPrecioDepartamentoMasCaro edificio = foldl (calcularYElegirMasCaro edificio) 0 (concatMap departamentos.pisos $ edificio)

{-
4. Remodelaciones... como somos cool, las nombramos en inglés:
a. Merge: Dada una lista de departamentos, nos devuelve uno nuevo “unificado”, con la superficie total de los anteriores y la habitabilidad promedio.
-}

calcularSuperficieTotal = sum . map superficie

calcularHabitabilidadPromedio departamentos = ((/).length $ departamentos) . sum . map habitabilidad $ departamentos

merge departamentos = Departamento (calcularSuperficieTotal departamentos) (calcularHabitabilidadPromedio departamentos)

{-
b. Split: Dado una cantidad y un departamento, nos da una lista de departamentos resultantes de dividir el anterior en esa cantidad, con la superficie homogénea y la misma habitabilidad.
-}

modificarSuperficie funcion departamento = departamento {superficie = funcion . superficie $ departamento}

split cantidad = (replicate cantidad) . modificarSuperficie (/7)

{-
5. Las catástrofes están a la orden día en Ciudad Batracia y afectan a los edificios, por lo que no podemos omitir sus efectos en nuestro modelo:
a. Incendio: Se produce desde un piso en particular, afectando a este y todos los pisos superiores. Reduce la habitabilidad de los departamentos afectados en 30 puntos porcentuales y la robustez del edificio se reduce a la mitad.
-}

modificarHabitabilidad :: (Number -> Number) -> Departamento -> Departamento
modificarHabitabilidad funcion departamento = departamento {habitabilidad = funcion . habitabilidad $ departamento}

modificarRobustez funcion edificio = edificio {robustez = funcion . robustez $ edificio}

modificarDepartamentos funcion piso = piso {departamentos = funcion . departamentos $ piso}

modificarPisos :: ([Piso] -> [Piso]) -> Edificio -> Edificio
modificarPisos funcion edificio = edificio {pisos = funcion . pisos $ edificio}

reducirHabitabilidadEn variable departamento
 | (>=variable).habitabilidad $ departamento = modificarHabitabilidad ((-)variable) departamento
 | otherwise = modificarHabitabilidad (*0) departamento

incendiarDepartamentos = modificarDepartamentos (map (reducirHabitabilidadEn 30))

incendiarPisoEdificio piso pisosEdificio = cambiarElemento (numero piso) (incendiarDepartamentos piso) pisosEdificio

incendiarSiEsNecesario numeroPiso piso
 | (==numeroPiso).numero $ piso = incendiarDepartamentos piso
 | otherwise = piso

incendio :: Number -> Edificio -> Edificio
incendio numeroPiso edificio = modificarRobustez (/2) . modificarPisos (map (incendiarSiEsNecesario numeroPiso)) $ edificio

{-
b. Plaga: La plaga afecta a un piso del edificio dado por su número y reduce la habitabilidad de sus departamentos en una cantidad de puntos porcentuales variable.
-}

aplicarPlaga :: Number -> Number -> [Piso] -> [Piso]
aplicarPlaga numeroPiso porcentaje pisosEdificio = cambiarElemento numeroPiso (modificarDepartamentos (map (reducirHabitabilidadEn porcentaje)) (pisosEdificio!!numeroPiso)) pisosEdificio

plaga numeroPiso porcentaje edificio = modificarPisos (aplicarPlaga numeroPiso porcentaje) edificio

{-
c. Terremoto: Reduce la robustez del edificio en un valor indicado.
-}

terremoto valor edificio
 | (>valor).robustez $ edificio = modificarRobustez ((-)valor) edificio
 | otherwise = modificarRobustez (*0) edificio

{-
6. De la mano de las catástrofes, llegan los arreglos o mejoras al edificio:
a. Ampliación: se realiza la construcción de un nuevo piso con una determinada cantidad de departamentos y de metros, que se reparten equitativamente entre los departamentos. El piso se agrega arriba, porque las máquinas para levantar los N pisos superiores de un edificio las tenemos descompuestas y no podemos meterlo en el medio. Al ser nuevo, su habitabilidad es de 100.
-}

{-
b. Fumigación: A cada departamento que tiene habitabilidad menor a 60%, les sube en 20 puntos porcentuales.
-}

fumigarDepartamento departamento 
 | (<60) . habitabilidad $ departamento = modificarHabitabilidad (+20) departamento
 | otherwise = departamento

fumigarPiso = modificarDepartamentos (map (fumigarDepartamento))

fumigacion = modificarPisos (map (fumigarPiso))

{-
c. MergeEdificio: Debe aplicar un merge sobre un número de piso dado de un edificio.
-}

aplicarMergeAPisoSiEsNecesario numeroPiso piso
 | (==numeroPiso).numero $ piso = modificarDepartamentos ((:[]) . merge) piso

mergeEdificio numeroPiso edificio = modificarPisos (map (aplicarMergeAPisoSiEsNecesario numeroPiso))

{-
d. SplitEdificio: Recibe una cantidad de nuevos departamentos y el número de piso donde debe hacer un split sobre el último departamento.
-}


