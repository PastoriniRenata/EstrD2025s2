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


{-PARA IR DESCOMENTANDO Y PROBANDO BAJAR ESTA LLAVE A MEDIDA Q VAN CODEANDO

heapSort :: Ord a => [a] -> [a]


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Implementar como usuario del tipo abstracto Map las siguientes funciones:
-}
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]



-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v






-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.

agruparEq :: Eq k => [(k, v)] -> Map k [v]





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int






-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
--OBS: assoc pisa el valor si la clave ya existe en el map

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v




-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar :: [a] -> Map Int a





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int





UNA VEZ QUE YA CODEARON TODO, ELIMINAR ESTA LLAVE --> -}






fromJust :: Maybe v -> v
--Precondicion: unicamente toma valores Just
fromJust (Just x) = x


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

