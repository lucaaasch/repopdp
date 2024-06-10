import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

 --El Congreso de la Nación sanciona diferentes leyes cada año, algunas más extensas o importantes que otras, pero en particular más que el detalle de su articulado nos interesa conocer cierta información clave, como ser el tema que trata, el presupuesto que requiere su implementación y cuáles son los partidos políticos, grupos de poder u otros sectores que la impulsaron o la apoyan. 
data Ley = UnaLey {
    tema :: String,
    presupuesto :: Number,
    partidos :: [String]
} deriving (Show, Eq)

leyCannabis = UnaLey{tema = "Uso Medicinal de Cannabis", presupuesto = 5, partidos = ["Cambio de Todos"]}
leyEducacionSuperior = UnaLey{tema = "Educacion Superior", presupuesto = 30, partidos = ["Docentes universitarios", "Partido de Centro Federal"]}
leyProfesionalizacion = UnaLey{tema = "Profesionalizacion del Tenis", presupuesto = 1, partidos = ["Partido de Centro Federal", "Liga de Deportistas Autonomos", "Club Paleta Veloz"]}
leyTenis = UnaLey{tema="Tenis", presupuesto = 2, partidos = ["Liga de Deportistas Autonomos"]}
--ley de prueba
leyConservadora = UnaLey{tema="Perpetuación del conservadurismo", presupuesto = 1000, partidos = ["Partido Conservador", "La Concordancia"]}

--LEYES.
--Averiguar si dos leyes son compatibles, que se da cuando tienen al menos un sector en común que la apoya y el tema de una de las leyes esté incluido en el otro. Por ejemplo, son compatibles la ley de “profesionalización del tenista de mesa” y la de “tenis”.
leyesCompatibles :: Ley -> Ley -> Bool
leyesCompatibles ley1 ley2 = interseccionNoVacia (partidos ley1) (partidos ley2) && interseccionNoVacia (tema ley1) (tema ley2)

interseccion lista1 lista2 = [x | x <- lista1, elem x lista2]

interseccionNoVacia lista1 lista2 = interseccion lista1 lista2 /= []

--CONSTITUCIONALIDAD DE LAS LEYES.
--La corte establece si una determinada ley es constitucional o no.
--De los jueces nos interesa saber como votan.
type CorteSuprema = [Juez]

data Juez = UnJuez {
    criterioDeVotacion :: Ley->Bool
} deriving (Show, Eq)

--Uno de los jueces se basa en la opinión pública: si el tema de la ley está en agenda, seguro que la declara constitucional. (Se conoce el conjunto de temas en agenda)
criterioOpinionPublica :: Ley -> Bool
criterioOpinionPublica ley = elem ley agenda

--Otro de los jueces, cuando se entera que la ley fue apoyada por el sector financiero, es imposible que la declare inconstitucional.
criterioFinanciero :: Ley -> Bool
criterioFinanciero ley = elem "sector financiero" (partidos ley)

--Hay un juez muy preocupado por las arcas del estado que declara inconstitucionales las leyes que requieren un presupuesto de más de 10 unidades.
--Existe otro juez con mentalidad similar pero un poco más tolerante que las declara inconstitucional recién si superan las 20 unidades de presupuesto.
criterioPresupuesto :: Number -> Ley -> Bool
criterioPresupuesto tope ley = ((<tope).presupuesto) ley

--Y el último de los jueces actuales decide declarar constitucional a toda ley que haya sido apoyada por el partido conservador
criterioConservador :: Ley -> Bool
criterioConservador ley = elem "partido conservador" (partidos ley)

agenda :: [Ley]
agenda = [leyCannabis, leyTenis]

corteSupremaActual :: CorteSuprema
corteSupremaActual = [juezOpinionPublica, juezFinanciero, juezBajoPresupuesto, juezPresupuestoModerado, juezConservador]

juezOpinionPublica = UnJuez{criterioDeVotacion = criterioOpinionPublica}
juezFinanciero = UnJuez{criterioDeVotacion = criterioFinanciero}
juezBajoPresupuesto = UnJuez{criterioDeVotacion = criterioPresupuesto 10}
juezPresupuestoModerado = UnJuez{criterioDeVotacion = criterioPresupuesto 20}
juezConservador = UnJuez{criterioDeVotacion = criterioConservador}

-- 1) Hacer que una Corte Suprema determine si se considera constitucional o no una ley.
juezVota :: Ley -> Juez -> Bool
juezVota ley juez= (criterioDeVotacion juez) ley

--ESTA ES LA FUNCION QUE USA LA CORTE SUPREMA PARA VOTAR-----------------------
corteSupremaVota :: CorteSuprema -> Ley -> Bool
corteSupremaVota corte ley = (mayoriaVotosPositivos.map (juezVota ley)) corte
-------------------------------------------------------------------------------
mayoriaVotosPositivos :: [Bool] -> Bool
mayoriaVotosPositivos votos = ((length.filter (==True)) votos) > (length votos / 2)

-- 2) Agregar nuevos jueces que puedan integran la corte suprema:

--Uno que siempre vote afirmativamente.
criterioAfirmativo :: Ley -> Bool
criterioAfirmativo ley = True

juezAfirmativo = UnJuez {criterioDeVotacion = criterioAfirmativo}

--Un juez inventado, con lógica totalmente diferente (no trivial).
-- Si está apoyado por menos de 3 sectores/partidos y el tema no está en la agenda, el juez declara la ley constitucional.
criterioContraLaCorriente :: Ley -> Bool
criterioContraLaCorriente ley = ((<3).length) (partidos ley) && not (elem ley agenda)

juezInventado = UnJuez {criterioDeVotacion = criterioContraLaCorriente}

--Otro juez que también tenga preocupación presupuestaria pero con otro importe.
juezPresupuestoRelajadisimo = UnJuez{criterioDeVotacion = criterioPresupuesto 1000}

-- 3) Hacer una función que dada una serie de leyes, averigue cuáles que no serían consideradas constitucionales con la actual conformación de la corte sí lo serían en caso de agregarle un conjunto de nuevos integrantes. 

leyesPrueba :: [Ley]
leyesPrueba = [leyCannabis, leyEducacionSuperior, leyProfesionalizacion, leyTenis]

corteSupremaNueva :: CorteSuprema
corteSupremaNueva = [juezOpinionPublica, juezFinanciero, juezBajoPresupuesto, juezPresupuestoModerado, juezConservador, juezAfirmativo, juezInventado, juezPresupuestoRelajadisimo]

leyesQueCumplen filtro leyes corte = filter (filtro) leyes

filtroConstitucional corte = corteSupremaVota corte
filtroInconstitucional corte = not.corteSupremaVota corte

leyesConstitucionales leyes corte = leyesQueCumplen (filtroConstitucional corte) leyes corte
leyesInconstitucionales leyes corte = leyesQueCumplen (filtroInconstitucional corte) leyes corte

leyesConstitucionalesConNuevaCorte :: [Ley] -> CorteSuprema -> [Juez] -> [Ley]
leyesConstitucionalesConNuevaCorte leyes corteActual juecesNuevos = interseccion (leyesInconstitucionales leyes corteActual) (leyesConstitucionales leyes (corteActual ++ juecesNuevos))


{--CUESTIÓN DE PRINCIPIOS
1 ) Hacer la función borocotizar, que dada una conformación de la Corte Suprema pasen a votar de forma contraria a lo que votaban antes y de esta manera, para cualquier ley, se cumpla que: 

constitucionalidad corteSuprema unaLey != constitucionalidad (borocotizar corteSuprema) unaLey

En mi caso la funcion "constitucionalidad" se llama corteSupremaVota así que la igualdad sería:
corteSupremaVota corteSuprema unaLey != corteSupremaVota (borocotizar corteSuprema) unaLey --}

borocotizar :: CorteSuprema -> CorteSuprema
borocotizar corte = map votarDeFormaContraria corte  

votarDeFormaContraria ::  Juez -> Juez
votarDeFormaContraria juez = juez {criterioDeVotacion = not.criterioDeVotacion juez}

--2) Determinar si un juez curiosamente coincide en su posición con un sector social, que se da cuando de todas las leyes actualmente en tratamiento, sólo vota leyes apoyadas por dicho sector.

juezCoincideConSectorSocial :: Juez -> [Ley] -> String -> Bool
juezCoincideConSectorSocial juez leyes sector = all (leyTieneSectorSocial sector)  (filter (flip juezVota juez) leyes)

leyTieneSectorSocial :: String -> Ley -> Bool
leyTieneSectorSocial sector ley = elem sector (partidos ley)

{-- PARA PENSAR
Si hubiera una ley apoyada por infinitos sectores ¿puede ser declarada constitucional? ¿cuáles jueces podrián votarla y cuáles no? Justificar

Si una ley estuviese apoyada por infinitos sectores podríaser votada por los jueces:
juezOpinionPublica, ya que solamente checkea que el tema de la ley esté en la agenda
juezBajoPresupuesto,juezPresupuestoModerado y juezPresupuestoRelajadisimo, debido a que solamente se fijan en el presupuesto de la ley.
juezAfirmativo, siempre vota de forma positiva, sin importar los sectores que la apoyen.

Por otro lado, los jueces que tendrían problemas con la ley apoyada por infinitos sectores serían:
juezInventado, ya que el criterio que le impuse fue que la ley esté apoyada por menos de 3 sectores y para evaluar esto aplica (<3).length, es decir que primero aplica length a la lista y luego se fija si está conformada por menos de 3 elementos. En este caso, como la lista de sectores es infinita, length nunca devolvería nada porque quedaría infinitamente contando la cantidad de elementos y como length nunca devuelve un resultado concreto, tampoco se puede evaluar si este resultado es menor a tres. En resumen, el juez inventado no podría declarar la ley como constitucional o inconstitucional.
juezConservador y juezFinanciero tendrían problemas.
el juezConservador evalua si el "partido conservador" está en la lista de sectores que apoyan una ley para votarla afirmativamente. Si la lista infinita de sectores fuese de la forma
["partido conservador", "partido conservador", "partido conservador", ..] (es decir, que el sector "partido conservador" se repite infinitamente)
el juez podría votarla afirmativamente. Si la lista fuese de otra forma, nunca podría determinar su voto debido a que nunca terminaría de evaluar si el "partido conservador" forma o no parte de esta lista. 

Lo mismo ocurre con el juezFinanciero, solamente que la lista de infinitos sectores de la ley tendría que tomar la forma
["partido conservador", "sector financiero", partido conservador", "sector financiero" ..] y solo podría votarla afirmativamente.

Debido a esto una ley apoyada por infinitos sectores solo podría ser declarada constitucional si la corte está conformada por los jueces mencionados anteriormente que no tienen problemas con este tipo de leyes. También podría estar conformada por los jueces sin problemas y además por el juezConservador O  el juezFinanciero, pero solamente si la lista de infinitos sectores toma la forma mencionada para cada caso--}