--  1. RECURSION SOBRE LISTAS
-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud []       = 0
longitud (_ : xs) = 1 + longitud xs

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1 : sucesores xs


-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion   []   = True
conjuncion (x:xs) = x && conjuncion xs


-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion   []   = False
disyuncion (x:xs) = x || disyuncion xs


-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss


-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece y (x:xs) = y==x || pertenece y xs


-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino (e==x) + apariciones e xs


--FUNCION AUXILIAR TRANSFORMA BOOLEANOS EN NROS
unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino   _  = 0 



-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _   []   = []
losMenoresA n (x:xs) = if x<n then x : losMenoresA n xs
                              else losMenoresA n xs
                       
                      

-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n    []    = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n then xs : lasDeLongitudMayorA n xss
                                                    else lasDeLongitudMayorA n xss



-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al ?nal de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal   []    y = [y]
agregarAlFinal (x:xs)  y = x : agregarAlFinal xs y


-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. De?nida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar   []   ys = ys 
agregar (x:xs) ys = x: agregar xs ys

-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa   []   = []
reversa (x:xs) = reversa xs  ++ [x]


-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys         = ys 
zipMaximos xs []         = xs
zipMaximos (x:xs) (y:ys) = if x>y then x : zipMaximos xs ys 
                                  else y : zipMaximos xs ys
    


-- Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
-- PRECONDICION : LA LISTA NO ES VACÍA
elMinimo   [x]  = x 
elMinimo (x:xs) = elMenor x (elMinimo xs)

elMenor:: Ord a => a -> a -> a
elMenor x y = if x< y
                 then x
                 else y






-- 2. RECURSION SOBRE NUMEROS
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = if n<0 then error "n tiene que ser mayor o igual a cero"
                     else n * factorial (n-1)

--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n<=0 then []
                            else n : cuentaRegresiva (n-1)

-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e


-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0  _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs


-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs 
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 





-- 3. REGISTROS
{-   
    3.1
    Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
    
-}

data Persona = P String Int
    deriving Show

persona_1 = P "A" 20
persona_2 = P "B" 30
persona_3 = P "C" 45
persona_4 = P "D" 25

-- [persona_1, persona_2, persona_3, persona_4]
-- OBSERVADORAS
edadDe :: Persona -> Int
edadDe (P _ e) = e



--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _   []   = []
mayoresA n (p:ps) = if esMayorA p n then p : mayoresA n ps
                                    else mayoresA n ps                    

--FUNCION AUXILIAR
esMayorA :: Persona -> Int -> Bool
esMayorA (P _ e) n = e>n



-- Dada una lista de personas devuelve el promedio de edad entre esas personas. 
promedioEdad :: [Persona] -> Int
--Precondición: la lista al menos posee una persona.
promedioEdad [] = error "No hay personas en la lista"
promedioEdad ps = div (sumatoriaEdades ps) (longitud ps)

-- FUNCION AUXILIAR
sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades [] = 0
sumatoriaEdades (p:ps) = edadDe p + sumatoriaEdades ps



-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "NO HAY PERSONAS!!"
elMasViejo [p] = p
elMasViejo (p: ps) = elMayorEntre p (elMasViejo ps)
    
    
elMayorEntre :: Persona -> Persona -> Persona
elMayorEntre p1 p2 = if esMasViejo p1 p2
                        then p1
                        else p2

esMasViejo :: Persona -> Persona -> Bool
esMasViejo p1 p2 = edadDe p1 > edadDe p2


{-
    3.2
    Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:
-}

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

poke_1 = ConsPokemon Agua 50
poke_2 = ConsPokemon Fuego 48
poke_3 = ConsPokemon Agua 32
poke_4 = ConsPokemon Fuego 32
poke_5 = ConsPokemon Fuego 32
poke_6 = ConsPokemon Planta 32
poke_7 = ConsPokemon Planta 32
entrenador_1 = ConsEntrenador "A" [poke_3, poke_1, poke_6]
entrenador_2 = ConsEntrenador "B" [poke_2, poke_4]
entrenador_3 = ConsEntrenador "C" [poke_2]
entrenador_4 = ConsEntrenador "D" [poke_1, poke_6]
entrenador_5 = ConsEntrenador "E" [poke_1, poke_3]

entr_6 = ConsEntrenador "F" [poke_1, poke_2, poke_6]


--FUNCIONES OBSERVADORAS 
tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _) = t

losPokesDe :: Entrenador -> [Pokemon]
losPokesDe (ConsEntrenador _ pks) = pks


-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
-- Definir en base a esa representación las siguientes funciones:

-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pks) = longitud pks

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ pks) = cantPokemonDeTipo t pks


-- FUNCIONES AUXILIARES
cantPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipo _   []   = 0
cantPokemonDeTipo t (p:ps) = unoSiEsDeTipo t p + cantPokemonDeTipo t ps


unoSiEsDeTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDeTipo t1 (ConsPokemon t2 _ ) = if esMismoTipo t1 t2 then 1
                                                            else 0

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua     = True
esMismoTipo Fuego Fuego   = True
esMismoTipo Planta Planta = True
esMismoTipo    _      _   = False




-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cuantosDe_De_LeGananATodosLosDe_ t (losPokesDe e1) (losPokesDe e2)
 
--FUNCIONES AUXILIARES
cuantosDe_De_LeGananATodosLosDe_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
-- Dado un tipo de pokemon y 2 listas de pokemones, indica cuantos de la primera lista le ganan a TODOS los de la 2da lista
cuantosDe_De_LeGananATodosLosDe_ t   []    _  = 0
cuantosDe_De_LeGananATodosLosDe_ t (p:ps) pks = unoSiCeroSino (esTipoSuperiorATodos (tipoDe p) pks) + cuantosDe_De_LeGananATodosLosDe_ t ps pks


esTipoSuperiorATodos :: TipoDePokemon -> [Pokemon] -> Bool
-- PROPOSITO: indica si el tipo dado es superior al tipo de TODOS los pokemones de la lista dada
esTipoSuperiorATodos _   []   = True
esTipoSuperiorATodos t (p:ps) =  esTipoSuperior t (tipoDe p) && esTipoSuperiorATodos t ps


esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
-- Proposito: Indica si el primer tipo es superior al segundo. Donde: Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
esTipoSuperior Agua Fuego   = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _    _       = False
 




-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pks) = hayPokeDeTipo Agua pks && hayPokeDeTipo Planta pks && hayPokeDeTipo Fuego pks


--FUNCION AUXILIAR
hayPokeDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayPokeDeTipo _   []   = False
hayPokeDeTipo t (p:ps) = esMismoTipo t (tipoDe p) || hayPokeDeTipo t ps







{-
    3.3
    El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
    de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
    una lista de personas con diferente rol. La de?nición es la siguiente:
-}
data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol]
    deriving Show



proy1 = ConsProyecto "Proy 1"
proy2 = ConsProyecto "Proy 2"
proy3 = ConsProyecto "Proy 3"
proy4 = ConsProyecto "Proy 4"

rol1 = Developer Junior proy1
rol2 = Management Senior proy2
rol3 = Developer SemiSenior proy3
rol4 = Developer Senior proy1
rol5 = Developer Senior proy1

empresa = ConsEmpresa [rol1, rol2, rol3, rol4]

empresaSinRepetidos = ConsEmpresa [rol1, rol2, rol3]

empresaConRepetidos = ConsEmpresa [rol1, rol4]

empresaSeniors = ConsEmpresa [rol2, rol4, rol5]

{-
losDevSenior empresa [proy1, proy2, proy3, proy4] --> 2
losDevSenior empresa [] --> 0
losDevSenior empresaConRepetidos [proy2, proy3] --> 0
losDevSenior empresaSeniors [proy2] --> 1
losDevSenior empresaSeniors [proy1] --> 2


---------------------------------------------------------------------------------------------------------------------

cantQueTrabajanEn [proy1, proy2, proy3, proy4] empresa--> 4
cantQueTrabajanEn [proy1, proy2] empresa --> 3
cantQueTrabajanEn [proy1, proy2, proy3, proy4] empresaSeniors --> 3

---------------------------------------------------------------------------------------------------------------------

asignadosPorProyecto empresa ---> [(proy1, 2), (proy2, 1), (proy3, 1)]
asignadosPorProyecto empresaSinRepetidos ---> [(proy1, 1), (proy2, 1), (proy3, 1)]
asignadosPorProyecto empresaConRepetidos ---> [(proy1, 2)]
asignadosPorProyecto empresaSeniors ---> [(proy1, 2), (proy2, 1)]

-}

--FUNCIONES OBSERVADORAS
elProyectoDe:: Rol -> Proyecto
elProyectoDe (Developer _ p) = p
elProyectoDe (Management _ p) = p


nombreDelProy :: Proyecto -> String
nombreDelProy (ConsProyecto n) = n
 


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Definir las siguientes funciones sobre el tipo Empresa:

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = losProyectos rs

-- FUNCIONES AUXILIARES
losProyectos :: [Rol] -> [Proyecto]
losProyectos   []   = []
losProyectos (r:rs) = agregarSiNoPertenece (elProyectoDe r) (losProyectos rs)

agregarSiNoPertenece :: Proyecto -> [Proyecto] -> [Proyecto] --agrega el proyecto al final en caso de no encontrarlo
agregarSiNoPertenece p ps = if elProyectoPertenece p ps
                                        then ps
                                        else p : ps

elProyectoPertenece :: Proyecto -> [Proyecto] -> Bool
--PROPOSITO: Dado un proyecto indica si pertenece a la lista de proyectos dada
elProyectoPertenece x   []   = False
elProyectoPertenece x (p:ps) = esMismoProyecto x p || elProyectoPertenece x ps

esMismoProyecto :: Proyecto -> Proyecto -> Bool
--PROPOSITO: dados dos proyectos, indica si son el mismo
esMismoProyecto p1 p2 = nombreDelProy p1 == nombreDelProy p2

{-
COSTO
esMismoProyecto --> O(1)
elProyectoPertenece --> O(P), donde P es la cantidad de proyectos NO REPETIDOS de la empresa
agregarSiNoPertenece --> O(P), por utilizar elProyectoPertenece
elProyectoDe --> O(1)

losProyectos --> O(R * P), ya que por cada Rol de la empresa se utiliza la funcion agregarSiNoPertenece, la cual tiene costo O(P)



-}



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = losSenior rs ps

-- FUNCIONES AUXILIARES 
losSenior :: [Rol] -> [Proyecto] -> Int
-- Proposito: Dada una lista de roles y una lista de proyectos, indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losSenior   []   ps = 0
losSenior (r:rs) ps = unoSiCeroSino (esDevSenior r &&  elProyectoPertenece (elProyectoDe r) ps) + losSenior rs ps



esDevSenior :: Rol -> Bool
esDevSenior (Developer s _)  = esSenior s
esDevSenior (Management s _) = esSenior s

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior    _   = False

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantQueTrabajanEn' ps rs


cantQueTrabajanEn' :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEn'  _   []   = 0
cantQueTrabajanEn' ps (r:rs) = unoSiCeroSino (elProyectoPertenece (elProyectoDe r) ps) + cantQueTrabajanEn' ps rs


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = cantPorProyecto rs


cantPorProyecto :: [Rol] -> [(Proyecto, Int)]
cantPorProyecto   []   = []
cantPorProyecto (r:rs) = sumarUnoEn (elProyectoDe r) (cantPorProyecto rs)


sumarUnoEn :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
sumarUnoEn p   []   = [(p, 1)]
sumarUnoEn p (t:ts) = if esTuplaDeMismoProy p t then (sumarUno t) : ts
                                                else t : sumarUnoEn p ts

esTuplaDeMismoProy :: Proyecto -> (Proyecto, Int) -> Bool
esTuplaDeMismoProy p1 (p2, _ ) = esMismoProyecto p1 p2

sumarUno :: (Proyecto, Int) -> (Proyecto, Int)
sumarUno (p, n) = (p, n+1)


{-
P: es la cantidad de proyectos distintos en la empresa
R: cantidad de roles en la empresa
sumarUno --> O(1) 
esTuplaDeMismoProy --> O(1)
sumarUnoEn --> Como la lista de tuplas, en peor caso, hay una tupla por cada proyecto distinto en la empresa y se hace un paso recursivo por tupla en la lista --> O(P)
cantPorProyecto --> en cada paso recursivo se urilizan elProyectoDe O(1) y sumarUnoEn O(P), se hacen R pasos recursivos --> O(R * P)
asignadosPorProyecto --> O(R * P)
-}




