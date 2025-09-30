import PriorityQueue
import Map

--de mutiset no hay funciones d usuario, en su archivo hay cositas para probarlo

{-
Ejercicio 2
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
práctica.
-}


pq = insertPQ 5 (insertPQ 4 (insertPQ 3 (insertPQ 2 (insertPQ 1 emptyPQ))))

--(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)


heapSort :: Ord a => [a] -> [a]
heapSort ls = pqALista (listaAPq ls)


pqALista :: PriorityQueue a -> [a]
pqALista pq = if isEmptyPq pq then []
                              else findMinPQ pq : (pqALista (deleteMinPq pq))

listaAPq :: [a] -> PriorityQueue a 
listaAPq   []   = emptyPQ
listaAPq (x:xs) = insertPq x (listaAPq xs)

{-
COSTO

listaAPq:
    - Siendo N la cantidad de elementos en la lista ingresada por argumento
    - insertPq tiene costo O(M), donde M es la cantidad de elementos en la PQ donde se quiere insertar el elemento
        -> Como en peor caso se quiere ingresar un elemento en la PQ que tiene (n-1) elementos, osea que estoy
            insertando el primer elemento de la lista original ==> M = N
        
        ==> insertPq tiene costo O(N)
    ==> listaAPq tiene costo O(N*N) ya que se hace uso de una funcion lineal (insertPq) en cada paso recursivo, 
        donde se hace un paso recursivo por cada elemento de la lista ingresada por argumento

-}



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Implementar como usuario del tipo abstracto Map las siguientes funciones:
-}
-- (Map, emptyM, assocM, lookupM, deleteM, keys)

--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = losValores (keys map) map

losValores :: Eq k => [k] -> Map k v 
losValores   []   map = []
losValores (k:ks) map = (lookupM k map) : losValores ks map

{-

-}

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas   []   map = True
todasAsociadas (c:cs) map = case lookupM k map of
                                Just v  -> todasAsociadas cs map
                                Nothing -> False



-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap    []    = emptyM
listToMap (cv:cvs) = let cv = (c,v)
                        in assocM c v (listToMap cvs)





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = tuplasClaveValor (keys map) map

tuplasClaveValor :: [k] -> Map k v -> [(k, v)]
tuplasClaveValor   []   map = []
tuplasClaveValor (c:cs) map = let v = fromJust (lookupM c) map
                                 in (c, v) : tuplasClaveValor cs map

fromJust :: Maybe v -> v
fromJust (Just x) = x
fromJust Nothing  = error "es un Valor de tipo Nothing"


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq   []     = emptyM
agruparEq (cv:cvs) = let (c,v) = cv
                         map = agruparEq cvs -- la recursión
                            in case lookupM c map in
                                Just xs -> assocM c (v:xs) map
                                Nothing -> assocM c [v] map

-- el assocM pisa valores!!!


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar   []    map = map
incrementar (c:cs)  map = case lookupM c map of
                            Just x -> assocM c (x+1) (incrementar cs map)
                            Nothing -> incrementar cs map




-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
--OBS: assoc pisa el valor si la clave ya existe en el map

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = unirMaps (keys map1) map1 map2
    
unirMaps ::Eq k => [k] -> Map k v -> Map k v -> Map k v
-- Proposito: dada la lista de claves del primer mapa, agrego todas las asociasiones clave-valor de dicho mapa al segundo.
unirMaps   []   map1 map2 = map2
unirMaps (c:cs) map1 map2 = let v = fromJust (lookupM c map1)
                                in assocM c v (unirMaps cs map1 map2)
                            

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar :: [a] -> Map Int a
indexar ls = indexarEn 0 ls

indexarEn :: Int -> [a] -> Map Int a
indexarEn _   []   = emptyM
indexarEn n (x:xs) = assocM n x (indexarEn (n+1) xs)


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias  ''   = emptyM
ocurrencias (c:cs)  = let map = ocurrencias cs
                        in case lookupM c map of
                            Just v  -> assocM c (v+1) map
                            Nothing -> assocM c   1   map


m1 :: Map Int String
m1 = assocM 1 "uno" (assocM 2 "dos" (assocM 3 "tres" (assocM 4 "cuatro" (assocM 5 "cinco" emptyM))))

lk :: [String]
lk = ["a", "b", "c", "d"]

m3 :: Map String Int
m3 = assocM "a" 1 (assocM "b" 1 (assocM "c" 1 (assocM "d" 1 emptyM)))

m4 :: Map String Int
m4 = assocM "g" 1 (assocM "h" 1 (assocM "c" 1 (assocM "d" 1 (assocM "a" 1 emptyM))))

m5 :: Map String Int
m5 = assocM "a" 2 (assocM "b" 2 (assocM "c" 2 (assocM "d" 2 emptyM)))

lkv :: [(String, String)]
lkv = [("a", "aa"), ("b", "bb"), ("c", "cc"), ("d", "dd"), ("e", "ee"), ("f", "ff"), ("g", "gg")]

-- >> valuesM m1
-- respuesta >> [Just "cinco",Just "cuatro",Just "tres",Just "dos",Just "uno"]

-- >> todasAsociadas [1,2,3,4,5,6,7] m1
-- respuesta >> False

-- >> todasAsociadas [1,3,4,5] m1
-- respuesta >> True

-- >> listToMap lkv
-- respuesta >> M [("g","gg"),("f","ff"),("e","ee"),("d","dd"),("c","cc"),("b","bb"),("a","aa")]

-- >> mapToList m1
-- respuesta >> [(5,"cinco"),(4,"cuatro"),(3,"tres"),(2,"dos"),(1,"uno")]

-- >> agruparEq [("a", 1), ("b", 2), ("a", 3), ("b", 4), ("c", 5)]
-- respuesta >> M [("c",[5]),("b",[2,4]),("a",[1,3])]
-- respuesta version 2 >> M [("a",[1,3]),("b",[2,4]),("a",[3]),("b",[4]),("c",[5])] 
--        está bien xq admite repetidos la estructura y como cuando haga lookup las 2das tuplas nunca me las va a devolver, está bien
--        trabaja de más al pedo, pero está bien

-- >> incrementar lk m3
-- respuesta >> M [("d",2),("c",2),("b",2),("a",2)]
--        => con V3  --> M ["d","c","b","a","a","b","c","d"] [2,2,2,2,1,1,1,1] xq admite repetidos, entonces agrega los que quiero incrementar con su nuevo valor y los otros quedan olvidados al fondo del map

-- >> incrementar lk m4
-- respuesta >> M [("d",2),("c",2),("a",2)]

m6 :: Map String Int
m6 = assocM "a" 2 (assocM "b" 2 (assocM "c" 2 (assocM "d" 2 emptyM)))

{-

m6 :: Map String Int
m6 = assocM "a" 2 (assocM "b" 2 (assocM "c" 2 (assocM "d" 2 emptyM)))

m4 :: Map String Int
m4 = assocM "g" 1 (assocM "h" 1 (assocM "c" 1 (assocM "d" 1 (assocM "a" 1 emptyM))))

mergeMaps m3 m4 --> M [("a",1),("b",1),("d",1),("c",1),("h",1),("g",1)]

-}

