
{-

1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
ordenes de complejidad en peor caso de cada función de la interfaz, justificando las respuestas.

-}

module MultiSet 
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) 
where

import Map

data MultiSet a = MS (Map a Int)
    deriving Show


{-
module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)

-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = MS emptyM

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS map) = aumentarUnaOcurrencia x map

aumentarUnaOcurrencia :: Ord a => a -> Map a Int -> Map a Int
aumentarUnaOcurrencia x map = case lookupM x map of
                                    Just n -> assocM x (n+1) map
                                    Nothing -> assocM x 1 map

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS map) = case lookupM x map of
                                    Just n -> n
                                    Nothing -> 0



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
unionMS x (MS map1) (MS map2) = M (unirApariciones (keys map1) map1 map2)

unirApariciones :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
-- Dadas las claves del primer map y un segundo map, agrega las apariciones del primer map en el 2do
unirApariciones   []   map1 map2 = map2
unirApariciones (c:cs) map1 map2 = let x = fromJust (lookupM c map1)
                                      ocurrs = ocurrenciasM c map1 + ocurrenciasM c map2
                                       map = unirApariciones cs map1 map2 -- la recursion
                                    in assoc c ocurrs map

ocurrenciasM :: Ord a => a -> Map a  Int -> Int
ocurrenciasM x map = case lookupM x map of
                                    Just n -> n
                                    Nothing -> 0
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
-- (osea el valor más chico, en caso de que uno de los elementos no este en el otro MS, devuelvo 0)
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
intersectionMS (MS map1) (MS map2) = MS (interseccionM (keys map1 ++ keys map2) map1 map2)

interseccionM :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
-- dada una lista con las claves que hay en AMBOS MAPS (admite repetidos), devuelve un map con la cantidad de ocurrencias que comparten
-- OSEA el valor más chico.
interseccionM   []     _    _  = emptyM
interseccionM (c:cs) map1 map2 = let ocurr1 = ocurrenciasM c map1 --en caso de que no haya un valor asociado a la clave, devuelve 0
                                     ocurr2 = ocurrenciasM c map2 --en caso de que no haya un valor asociado a la clave, devuelve 0
                                     min = elMasChico ocurr1 ocurr2
                                     map = interseccionM cs map1 map2 --recusrion
                                     in assocM c min map 
                                        
elMasChico :: Int -> Int -> Int
elMasChico x y = if x>y then y else x


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS map) = mapALista (keys map) map

mapALista :: Eq a => [a] -> Map a Int -> [(a, Int)]
mapALista   []    _  = []
mapALista (c:cs) map = 



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


ms1 :: MultiSet String
ms1 = MS m1

ms2 :: MultiSet String
ms2 = MS m2

m1 = assocM "a" 1 (assocM "b" 2 (assocM "c" 3 (assocM "d" 4 emptyM)))

m2 = assocM "a" 1 (assocM "f" 2 (assocM "c" 3 (assocM "h" 4 (assocM "i" 5 (assocM "j" 6 emptyM)))))


{-
addMS "e" ms1 ==> MS (M [("d",4),("c",3),("b",2),("a",1),("e",1)])
addMS "a" ms1 ==> MS (M [("d",4),("c",3),("b",2),("a",2)])

ocurrencesMS "a" ms1 ===> 1
ocurrencesMS "c" ms1 ===> 3

unionMS ms1 ms2 ==> MS (M [("d",4),("c",6),("b",2),("a",2),("f",2),("h",4),("i",5),("j",6)])


intersectionMS ms1 ms2 ==> MS (M [("c",6),("a",2)]) -- Tome como que tengo q sumar los valores de las claves q coinciden en ambos MS

multiSetToList ms1 ==> [("d",4),("c",3),("b",2),("a",1)]

-}


