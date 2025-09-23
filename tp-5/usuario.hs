import Set
import Queue
import Stack


{-
1. Cálculo de costos
Especificar el costo operacional de las siguientes funciones:
-}


head' :: [a] -> a -- O(1)
head' (x:xs) = x
{-
O(1), ya que abrir la estructura tiene costo constante
-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sumar :: Int -> Int      -- O(1)
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

{-
O(1), ya que al ser una cantidad finita de operaciones constantes, las mismas se absorben, por lo que termina teniendo costo constante

-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factorial :: Int -> Int  -- O(n) donde n es el numero ingresado por parametro, osea la cantidad de iteraciones 
factorial 0 = 1          -- que hace la funcion
factorial n = n * factorial (n-1)
{-
n -> el nro ingresado por argumento
O(n), ya que se hace una operacion de costo constante (*) en cada paso recursivo, y se hacen n pasos recursivos
-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

{-
N -> la cantidad de elementos en la lista ingresada por argumento
O(N), ya que se hace una operacion de costo constante (+) en cada paso recursivo, y se hacen N pasos recursivos, uno por cada elemento de la lista
-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
factoriales :: [Int] -> [Int] -- O(m * n)  donde m es el largo de la lista y n es el costo de usar la funcion factorial en cada iteración
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs
{-
factorial --> O(N)
M -> cantidad de elementos en la lista ingresada por argumento

como se hace una operacion de costo lineal por cada elemento de la lista --> se hacen M operaciones de costo lineal
                                                                        ==> factoriales tiene costo O(N * M)
-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool 
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

{-
N -> la cantidad de elementos en la lista ingresada por argumento
O(N), ya que se hace una operacion de costo constante (==) en cada paso recursivo, y en peor caso se hacen N pasos recursivos, 
      uno por cada elemento de la lista
-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                                then sinRepetidos xs
                                else x : sinRepetidos xs

{-
N es la cantidad de elementos en la lista ingresada

pertenece -> O(N) 

se hace una operacion de costo lineal (pertenece), y en peor caso una operacione de costo constante (:), en cada paso recursivo
y se hace un paso recursivo por cada elemento de la lista ingresada

==> sinRepetidos tiene costo O(N*N)

-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
{-

-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
{-

-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
{-
 
-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
{-
 
 -}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
{-

-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
{-

-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                       then xs
                       else x : sacar n xs
{-

-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs
                in m : ordenar (sacar m xs)
{-

-}








--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--SET


-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

set_Vacio = emptyS                      -- --> S []      0
set_1 = addS 1 emptyS                   -- --> S [1]     1       
set_2 = addS 2 (addS 1 emptyS)          -- --> S [1,2]   2      
set_3 = addS 3 (addS 2 (addS 1 emptyS)) -- --> S [1,2,3] 3   

tree_set :: Tree (Set Int)
tree_set = NodeT set_1 (NodeT set_Vacio (EmptyT) 
                                         (NodeT set_2 (EmptyT)
                                                      (NodeT set_3 EmptyT 
                                                                   EmptyT))) 
                        (EmptyT)

--  (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen   []   st = []
losQuePertenecen (x:xs) st = if belongs x st 
                                     then x : losQuePertenecen xs st
                                     else losQuePertenecen xs st

{-
losQuePertenecen [1,2,3,4,5,6] set_Vacio --> []               
losQuePertenecen [1,2,3,4,5,6] set_1     --> [1]                       
losQuePertenecen [1,2,3,4,5,6] set_2     --> [1,2]                       
losQuePertenecen [1,2,3,4,5,6] set_3     --> [1,2,3]                       
-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidosS :: Eq a => [a] -> [a]
sinRepetidosS ls = setToList (aSet ls)

aSet ::  Eq a => [a] -> Set a
aSet   []   = emptyS
aSet (x:xs) = addS x (aSet xs)
{-
sinRepetidos [1,2,3,4,5,6] --> [1,2,3,4,5,6]               
sinRepetidos [1,1,2,2,3,3] --> [1,2,3]                       
sinRepetidos [1,1,1,1,1,1] --> [1]                       
sinRepetidos [1,2,3,1,2,3] --> [1,2,3]                       
-}



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT            = emptyS
unirTodos (NodeT set t1 t2) = unionS set (unionS (unirTodos t1) (unirTodos t2))



{-
unirTodos tree_set --> S [1,2,3] 3
        ---> setToList (unirTodos tree_set ) --> [1,2,3]
        ---> sizeS (unirTodos tree_set )     --> 3
-}



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


--QUEUES
--(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

q1 = enqueue 1 (enqueue 2 (enqueue 3 emptyQ))
q2 = enqueue 11 (enqueue 22 (enqueue 33 emptyQ))
q3 = enqueue 11 (enqueue 22 (enqueue 33 q2))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q then 0 else 1 + lengthQ (dequeue q)


{-
lengthQ q1 --> 3
lengthQ q2 --> 3
lengthQ q3 --> 6
-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q then [] else (firstQ q) : queueToList (dequeue q)

{-
queueToList q1 --> [3,2,1]
queueToList q2 --> [33,22,11]
queueToList q3 --> [33,22,11,33,22,11]
-}
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2 then q1 else unionQ (enqueue (firstQ q2) q1 ) (dequeue q2) -- no puedo hacerlo por rec estructural xq cambia el orden 

{-
unionQ q1 q2 --> Q [3,2,1,33,22,11]
-}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- STACK
st1 :: Stack Int
st1 = push 4 (push 3 (push 2 (push 1 (push 0 emptySt))))

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar   []   = emptySt
apilar (x:xs) = push x (apilar xs)
{-
  apilar [1,2,3,4,5,6] >> resultado >> S [1,2,3,4,5,6] 6
-}


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar st = if isEmptySt st 
                        then []
                        else top st : desapilar (pop st)
{-
desapilar st1 >> resultado >> [4,3,2,1,0]
-}



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x st = push x st
insertarEnPos n x st = push (top st) (insertarEnPos (n-1) x (pop st))



{-
insertarEnPos 0 5 st1 >> resultado >> S [5,4,3,2,1,0] 6
insertarEnPos 5 5 st1 >> resultado >> S [4,3,2,1,0,5] 6
-}

