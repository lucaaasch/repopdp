module TP0 where
import PdePreludat

data Obra = Obra{
    texto :: String,
    año :: Number
} deriving (Show,Eq)

data Autor = Autor{
    nombre :: String,
    obras :: [Obra]
} deriving (Show,Eq)

obraA :: Obra
obraA = Obra {texto = "Había una vez un pato.", año = 1997}

obraB :: Obra
obraB = Obra {texto = "¡Había una vez un pato!", año = 1996}

unAutor :: Autor
unAutor = Autor {nombre = "Pato", obras = [obraA, obraB]}