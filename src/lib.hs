import PdePreludat 

-- LIBRERIA DE FUNCIONES LUCAS ---------------------------------------------------

-- Agregar un elemento al final de una lista
add :: [a] -> a -> [a]
add xs x = xs ++ [x]

-- Agregar la primera ocurrencia de un elemento en una lista (recursivamente)
-- Es la alternativa a delete

eliminarPrimeraOcurrencia :: Eq a => a -> [a] -> [a]
eliminarPrimeraOcurrencia _ [] = []
eliminarPrimeraOcurrencia x (y:ys)
    | x == y    = ys
    | otherwise = y : eliminarPrimeraOcurrencia x ys

-- Ver si una lista contiene a otra sublista
contieneSublista :: (Eq a) => [a] -> [a] -> Bool
contieneSublista sublista lista = all (`elem` lista) sublista

-- Eliminar todos los elementos de una sublista en una lista
eliminarListaDentroDeOtra :: Eq a => [a] -> [a] -> [a]
eliminarListaDentroDeOtra [] listaOriginal = listaOriginal
eliminarListaDentroDeOtra xs listaOriginal = foldl (flip eliminarPrimeraOcurrencia) listaOriginal xs

applyFirst :: (a -> a) -> [a] -> [a]
applyFirst _ []     = []
applyFirst f (x:xs) = f x : xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []  -- Caso base: si la lista está vacía, devuelve una lista vacía
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs  -- Si x cumple con el predicado p, lo agrega a la lista y sigue con el resto
    | otherwise = []                  -- Si x no cumple, detiene y devuelve la lista resultante hasta ahora

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-----------------------------------------------------------------------------------