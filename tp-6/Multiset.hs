
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



{-PARA IR DESCOMENTANDO Y PROBANDO BAJAR ESTA LLAVE A MEDIDA Q VAN CODEANDO
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int




--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]


UNA VEZ QUE YA CODEARON TODO, ELIMINAR ESTA LLAVE --> -}




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


