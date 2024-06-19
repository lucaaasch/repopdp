import PdePreludat

--------------------------------------------------------------------
-- PARCIAL: STAR WARS ----------------------------------------------
--------------------------------------------------------------------

-- Punto 1

data Nave = Nave{
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show,Eq)

type Poder = Nave -> Nave

cambiarAtaque :: (Number -> Number) -> Nave -> Nave
cambiarAtaque cambio nav = nav { ataque = cambio (ataque nav) }

cambiarDurabilidad :: (Number -> Number) -> Nave -> Nave
cambiarDurabilidad cambio nav = nav { durabilidad = cambio (durabilidad nav) }

cambiarEscudo :: (Number -> Number) -> Nave -> Nave
cambiarEscudo cambio nav = nav { escudo = cambio (escudo nav) }

turbo :: Poder
turbo = cambiarAtaque (+25)

reparacionEmergencia :: Poder
reparacionEmergencia = cambiarAtaque (subtract 30) . cambiarDurabilidad (+50)

superTurbo :: Poder
superTurbo = cambiarDurabilidad (subtract 45) . turbo . turbo . turbo

tieFighter :: Nave
tieFighter = Nave {nombre = "Tie Fighter", durabilidad = 200, escudo = 50, ataque = 50, poder = turbo}

xWing :: Nave
xWing = Nave {nombre = "X Wing", durabilidad = 300, escudo = 150, ataque = 100, poder = reparacionEmergencia}

naveDarthVader :: Nave
naveDarthVader = Nave {nombre = "Nave de Darth Vader", durabilidad = 500, escudo = 300, ataque = 200, poder = superTurbo}

milleniumFalcon :: Nave
milleniumFalcon = Nave {nombre = "Millenium Falcon", durabilidad = 1000, escudo = 500, ataque = 50, poder = cambiarEscudo (+100) . reparacionEmergencia}

nuevoPoder :: Poder
nuevoPoder = cambiarDurabilidad (subtract 100) . cambiarAtaque (+300) . turbo

nuevaNave :: Nave
nuevaNave = Nave {nombre = "Nueva Nave", durabilidad = 200, escudo = 100, ataque = 700, poder = nuevoPoder}

-- Punto 2

type Flota = [Nave]

flota :: Flota
flota = [tieFighter,xWing,naveDarthVader,milleniumFalcon,nuevaNave]

durabilidadTotal :: Flota -> Number
durabilidadTotal = sum . map durabilidad

-- Punto 3

calcularDaño :: Nave -> Nave -> Number
calcularDaño n1 n2 
    | escudoN2 > ataqueN1 = 0
    | otherwise = ataqueN1 - escudoN2

    where escudoN2 = escudo (activarPoder n2)
          ataqueN1 = ataque (activarPoder n1)

activarPoder :: Nave -> Nave
activarPoder nave = poder nave nave

atacar :: Nave -> Nave -> Nave
atacar n1 n2 = cambiarDurabilidad (max 0 . subtract (calcularDaño n1 n2)) n2

-- Punto 4

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

-- Punto 5

-- Flota , Nave , Estrategia (Nave -> Bool)

type Estrategia = Nave -> Bool

esDebil :: Estrategia
esDebil n = escudo n < 200

pocaDurabilidad :: Estrategia
pocaDurabilidad n = durabilidad n < 50

esPeligrosa :: Number -> Estrategia
esPeligrosa num nave = ataque nave > num

quedaFueraDeCombate :: Nave -> Estrategia
quedaFueraDeCombate n1 = fueraDeCombate . atacar n1 

misionSorpresa :: Nave -> Estrategia -> Flota -> Flota
misionSorpresa nave estrat flota = filter (not . estrat) flota ++ map (atacar nave) (filter estrat flota)

-- Punto 6

ejecutarMision :: Nave -> Estrategia -> Flota -> Flota
ejecutarMision = misionSorpresa

mejorEstrategia :: Nave -> Estrategia -> Estrategia -> Flota -> Estrategia
mejorEstrategia nav est1 est2 flota 
    | duraEst1 > duraEst2 = est1
    | otherwise = est2
    where duraEst1 = durabilidadTotal (misionSorpresa nav est1 flota)
          duraEst2 = durabilidadTotal (misionSorpresa nav est2 flota)

misionDobleEstrategia :: Nave ->  Flota -> Estrategia -> Estrategia  -> Flota
misionDobleEstrategia nave flota est1 est2  = ejecutarMision nave (mejorEstrategia nave est1 est2 flota) flota 

misionDobleEstrategia' :: Nave -> Estrategia -> Estrategia -> Flota -> Flota -> Flota
misionDobleEstrategia' nave est1 est2 = ejecutarMision nave . mejorEstrategia nave est1 est2 

-- Punto 7

flotaInfinita :: Flota
flotaInfinita = repeat xWing

{-
    No es posible determinar su durabilidad total ya que para hacerlo, es necesario terminar de ejecutarla. Lo mismo con la mision, para determinar la estrategia necesitamos evaluar toda la flota.
-}

