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

