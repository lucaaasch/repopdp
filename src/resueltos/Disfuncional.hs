type Mascota = (String, Int)

tieneNombreLargo mascota = length (fst mascota) > 9 == True
--tieneNombreLargo mascota = length (fst mascota) > 9
--tieneNombreLargo mascota = (> 9) . length . nombre $ mascota

data Persona = Persona {
  nombre  :: String,
  energia :: Int,
  edad    :: Int,
  hobbies :: [String]
}

data Persona = Persona String Int Int [String]

sumarEnergia (Persona _ energia _ _) = (Persona _ (energia + 5) _ _)
--sumarEnergia (Persona nombre energia edad hobbies) = Persona nombre (energia + 5) edad hobbies
--sumarEnergia unaPersona = unaPersona { energia = energia unaPersona + 5 }
--sumarEnergia unaPersona = modificarEnegia (+ 5) unaPersona


triplicarLosPares numeros = (map (* 3) . filter even) numeros


sonTodosMamiferos animales = all (==True) (map esMamifero animales)
--sonTodosMamiferos animales = (and . map esMamifero) animales
--sonTodosMamiferos animales = all esMamifero animales

type ReguladorDeTemperatura = Casa -> Casa

abrirVentanas :: ReguladorDeTemperatura
prenderEstufa :: ReguladorDeTemperatura
--encenderElAireA :: Casa -> Int -> Casa
encenderElAireA :: Int -> ReguladorDeTemperatura
mudarseA :: String -> ReguladorDeTemperatura

miCasaInteligente = Casa {
  direccion = "Medrano 951",
  temperatura = 26,
  reguladoresDeTemperatura = [
    abrirVentanas,
    prenderEstufa,
    mudarseA "Medrano 952",
    encenderElAireA 24
    -- flip encenderElAireA 24
  ]
}


esBeatle  "Ringo" = True
esBeatle   "John" = True
esBeatle "George" = True
esBeatle   "Paul" = True
esBeatle        _ = False

esBeatle unaPersona = elem unaPersona beatles



sumaDeLasEdadesRecursiva              [] = 0
sumaDeLasEdadesRecursiva (cabeza : cola) =
   edad cabeza + sumaDeLasEdadesRecursiva cola

sumaDeLasEdades unasPersonas =
  foldr (\unaPersona acumulador -> edad unaPersona + acumulador) 0 unasPersonas

sumaDeLasEdades unasPersonas = sum . map edad $ unasPersonas


abrirVentanas casa = casa { temperatura = temperatura casa - 2 }



agregarValor valor indice lista = take indice lista ++ valor : drop indice lista


poneleUnNombre numeros = (sum (map (*3) (filter even numeros))) < 100

sonZarpados :: [Int] -> Bool
sonZarpados numeros = (< 100) . sum . triplicarLosPares $ numeros
