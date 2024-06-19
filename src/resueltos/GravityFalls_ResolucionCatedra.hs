import Text.Show.Functions ()

type Item = String

data Persona = Persona {
  items :: [Item]
} deriving Show

data Criatura = Criatura {
  peligrosidad                  :: Int,
  condicionParaDeshacerseDeElla :: Persona -> Bool
} deriving Show

siempredetras :: Criatura
siempredetras = Criatura {
  peligrosidad = 0,
  condicionParaDeshacerseDeElla = const False
}

esUnMamiferoBipedo :: Persona -> Bool
esUnMamiferoBipedo unaPersona = esUnMamifero unaPersona && esBipedo unaPersona

esUnMamifero = undefined
esBipedo = undefined

gnomos :: Int -> Criatura
gnomos unaCantidad = Criatura {
  peligrosidad = 2 ^ unaCantidad,
  condicionParaDeshacerseDeElla = tiene "Soplador de hojas"
}

fantasma :: Int -> (Persona -> Bool) -> Criatura
fantasma unaCategoria asuntoPendiente = Criatura {
  peligrosidad                  = unaCategoria * 20,
  condicionParaDeshacerseDeElla = asuntoPendiente
}

tiene :: Item -> Persona -> Bool
tiene unItem unaPersona = elem unItem . items $ unaPersona

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf unaFuncion condicion (cabeza : cola) (otraCabeza : otraCola)
 | not . condicion $ otraCabeza =                   otraCabeza : zipWithIf unaFuncion condicion (cabeza : cola) otraCola
 | otherwise                    = unaFuncion cabeza otraCabeza : zipWithIf unaFuncion condicion cola otraCola



masLargoEntreMuchos :: [String] -> String
masLargoEntreMuchos muchosStrings = foldl1 masLargo muchosStrings

masLargo :: String -> String -> String
masLargo unString otroString
  | length unString > length otroString = unString
  | otherwise                           = otroString
