
module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

{-
Ejercicio 1
La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.
-}

data PriorityQueue a = Pq [a]
    deriving Show


{-PARA IR DESCOMENTANDO Y PROBANDO BAJAR ESTA LLAVE A MEDIDA Q VAN CODEANDO
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a -- O(1)



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool -- O(1)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía. --> La priority que no está vacía
findMinPQ :: Ord a => PriorityQueue a -> a -- O(1)



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.  --> La priority que no está vacía
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)





UNA VEZ QUE YA CODEARON TODO, ELIMINAR ESTA LLAVE --> -}






