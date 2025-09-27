
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


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a -- O(1)
emptyPQ = Pq []

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
isEmptyPQ (Pq ls) = esVacia ls

esVacia :: [a] -> Bool
esVacia [] = True
esVacia  _ = False


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(N), donde N es la cantidad de elementos en la PQ
insertPQ x (Pq ls) = insertarLs x ls

insertarLs :: Ord a => a -> [a] -> [a]
-- PROPOSITO: dado un elemento y una lista, retorna la lista modificada con el elemento insertado en la posici'on 
-- que le corresponda, en orden
insertarLs x [] = [x]
insertarLs x ls = let (y:ys) = ls 
                    if x<y then x:ls
                           else y: (insertarLs x ys)
                             



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a -- O(1)
findMinPQ (Pq ls) = finMinLs ls

findMinLs :: Ord a => [a] -> a
findMinLs   []   = error"la Priority Queue está vacía"
findMinLs (x:xs) = x


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.  --> La priority que no está vacía
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)
deleteMinPQ (Pq ls) = Pq (deleteMinLs ls)

deleteMinLs :: Ord a => [a] -> a
deleteMinLs   []   = error"la Priority Queue está vacía"
deleteMinLs (x:xs) = xs



