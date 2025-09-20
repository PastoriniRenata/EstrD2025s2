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
-- un conjunto que no tiene repetido. Ni para el usuario ni para el implementador
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Crea un conjunto vacío.
emptyS :: Set a
emptyS = S []

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S ls) = S (x:ls)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S ls) = elem x ls

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S ls) = longitud ls

longitud :: [a] -> Int
longitud   []   = 0
longitud (x:xs) = 1 + longitud xs

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Borra un elemento del conjunto. 
removeS :: Eq a => a -> Set a -> Set a
removeS x (S ls) = S (removeL x ls)

removeL :: Eq a => a -> [a] -> [a]
--elimina todas las repeticiones
removeL x   []   = []
removeL x (y:ys) = if x==y then ys
                           else y : removeL x ys


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS st (S ls)  = agregarAlSet ls st

agregarAlSet :: Eq a =>  [a] -> Set a -> Set a
agregarAlSet xs (S ys) = S (xs++ys)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S ls) = sinRepetidos ls

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos   []   = []
sinRepetidos (x:xs) = let ys = sinRepetidos xs
                        in if elem x ys
                            then ys
                            else x:ys



