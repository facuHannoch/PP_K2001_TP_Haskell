type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
patente :: Patente,
desgasteLlantas :: [Desgaste],
rpm :: Int,
temperaturaAgua :: Int,
ultimoArreglo :: Fecha
} deriving Show



-- Punto 1
obtCostoRep :: Auto -> Float
obtCostoRep (Auto patente _ _ _ _)
    | length patente == 7 = 12500
    | patente >= "DJ" && patente <= "NB" = calculoPatental patente
    | otherwise = 15000

calculoPatental :: Patente -> Float
calculoPatental patente
    | last patente == '4' = (*3000) . fromIntegral . length $ patente
    | otherwise = 20000

 

-- Punto 2
-- Usando únicamente Composición y aplicación parcial

esPeligroso :: Auto -> Bool
esPeligroso = (>0.5) . head . desgasteLlantas

necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015) . anio . ultimoArreglo



-- -- Punto 3
type Tecnico = Auto -> Auto
alfa :: Tecnico
alfa auto@(Auto _ _ rpm _ _)
    | rpm > 2000 = auto {rpm = 2000}
    | otherwise = auto
bravo :: Tecnico
bravo auto = auto {desgasteLlantas = [0, 0, 0, 0]}
charly :: Tecnico
charly = alfa . bravo

tango :: Tecnico
tango auto = auto
zulu :: Tecnico
zulu auto = lima auto {temperaturaAgua = 90}
lima :: Tecnico
lima auto = auto {desgasteLlantas = 0:0:drop 2 (desgasteLlantas auto)}



-- -- Punto 4
-- usar recursividad solamente

autosEstanOrdenados :: [Auto] -> Bool
autosEstanOrdenados autos = ordenados autos 1

ordenados :: [Auto] -> Int -> Bool
ordenados [] _ = True
ordenados (x:xs) index = ordenados xs (index + 1) && not (cumpleCriterio x index)

cumpleCriterio :: Auto -> Int -> Bool
cumpleCriterio auto index = mod index 2 /= mod (desgaste auto) 2

desgaste :: Auto -> Int
desgaste auto = calcDesgasteTotLlantas' (desgasteLlantas auto) 0

calcDesgasteTotLlantas' :: [Desgaste] -> Float -> Int
calcDesgasteTotLlantas' [] suma =  round . (*10) $ suma
calcDesgasteTotLlantas' (x:xs) suma = calcDesgasteTotLlantas' xs (x+suma)


-- -- Punto 5
-- ordenDeReparacion :: [Tecnico] -> Fecha -> Auto -> Auto
-- ordenDeReparacion [] fecha auto = auto {ultimoArreglo = fecha}
-- ordenDeReparacion (x:xs) fecha auto = ordenDeReparacion xs fecha (x auto)


ordenDeReparacion :: [Tecnico] -> Fecha -> Auto -> Auto
ordenDeReparacion tecnicos fechaModificacion auto = (aplicarTecnicos tecnicos auto) {ultimoArreglo = fechaModificacion}

aplicarTecnicos :: [Tecnico] -> Auto -> Auto
aplicarTecnicos tecnicos auto = foldl (\autoAcc tecnico -> tecnico autoAcc) auto tecnicos





-- -- Punto 6
-- únicamente funciones de orden superior
listaDeBuenosTecnicos :: [Tecnico] -> Auto -> [Tecnico]
listaDeBuenosTecnicos tecnicos auto = filter (\x -> not . esPeligroso . x $ auto) tecnicos

costosDeAutosAReparar :: [Auto] -> Float
costosDeAutosAReparar autos = foldl (\acc auto-> acc + obtCostoRep auto) 0 (autosAReparar autos)

autosAReparar :: [Auto] -> [Auto]
autosAReparar = filter necesitaRevision



-- -- Punto 7
-- Lazy Evaluation

tecnicosInfinitos :: [Tecnico]
tecnicosInfinitos = zulu:tecnicosInfinitos
autosInfinitos :: [Auto]
autosInfinitos = generarAutosInfinitos' 0

generarAutosInfinitos' :: Int -> [Auto]
generarAutosInfinitos' n = Auto {
    patente = "AAA000",
    desgasteLlantas = [fromIntegral n, 0, 0, 0.3],
    rpm = 1500 + n,
    temperaturaAgua = 90,
    ultimoArreglo = (20, 1, 2013)
} : generarAutosInfinitos' (n + 1)


-- A) En base al punto “dada una lista de técnicos determinar qué técnicos dejarían el auto en condiciones” y considerando una lista de técnicos infinita,¿podríamos obtener el primer técnico que deja el auto en condiciones? Muestre un ejemplo y justifique.
-- B) En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”, ¿podríamos tener una lista infinita de autos? Muestre un ejemplo y justifique. Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, ¿cómo debería cambiar la función? Por otra parte, ¿esta versión aceptaría una lista infinita de autos? Modifique la función 6.b con otro nombre y justifique sus respuestas.

-- RESPUESTA
-- Debido a que Haskell evalúa de forma perezosa, es posible el uso de listas infinitas, y por lo tanto, de funciones que aprovechen tales listas. No obstante, hay que tener cuidado ya que tales listas, por definición, no finalizarán jamás.
-- A) Sí, daba una lista de infinitos tecnicos, debido a que haskell espera y no evalúa toda la lista infinita, podemos obtener valores que cumplan la condición sin la necesidad de tener que evaluar la lista completa (que no se puede, ya que es infinita y por definición nunca va a terminar). No obstante, no se puede garantizar que se encuentre un valor que cumpla tal condición, con lo cual Haskell seguirá buscando hasta que encuentre uno.


aplicacionInfDeTec :: [Tecnico] -> Auto -> [Bool]
aplicacionInfDeTec tecnicos auto = takeWhile (==True) $ map (\x -> not . esPeligroso . x $ auto) tecnicos



-- B) Si quisiéramos obtener el costo total de los autos que necesitacen revisión, es decir de principio a fin, no sería posible, puesto que la lista nunca terminaría. No obstante, si la listas es infinita, pero tomamos un número finito de autos que encontramos a medida que vamos evaluando los términos, por ejemplo 3, en ese caso sí podríamos saber el costo de reparación total (sumando los tres autos). Esta versión si aceptaría una lista infinita de autos, pero la cantidad de términos evaluados siempre será finita.

costosDeAutosAReparar' :: [Auto] -> Float
costosDeAutosAReparar' autos = sum . take 3 $ costoDeConjFinitoDeAutosAReparar autos

costoDeConjFinitoDeAutosAReparar :: [Auto] -> [Float]
costoDeConjFinitoDeAutosAReparar = map obtCostoRep . filter necesitaRevision


