{-
3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.

-}



module Set
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] 
    deriving Show
 
    {- INV. REP. en S elem
       -- True --> no tiene invariante (corroborado con Fidel)
    -}

--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Crea un conjunto vacío.
emptyS :: Set a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Borra un elemento del conjunto. 
-- OBS mia: tengo que sacar todas las repeticiones del elemento
removeS :: Eq a => a -> Set a -> Set a



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]





