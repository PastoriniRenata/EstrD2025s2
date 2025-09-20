


module Queue
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where


{-
Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). Su interfaz es la siguiente:
-}
data Queue a = Q [a]
    deriving Show
{-
    - El pirmer elemento de la queue es el primer elemento ingresado en la lista. 
-}


{-
Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
final de la lista y desencolarse por delante.
-}
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Crea una cola vacía.
emptyQ :: Queue a -- O(1)
emptyQ = Q []

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool -- O(1)
isEmptyQ (Q ls) = esVacia ls 

esVacia :: [a] -> Bool
esVacia [] = True
esVacia  _ = False
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
-- O(N) por la utilización de agregarAlFianl
enqueue x (Q ls) = Q (agregarAlFinal x ls)

agregarAlFinal :: a -> [a] -> [a]  
-- O(N), donde N es la cantidad de elementos en la cola, ya que para poder agregar el elemento al final de la lista, se deben hacer N pasos recursivos, uno por cada elemento de la lista
agregarAlFinal x   []   = [x]
agregarAlFinal x (y:ys) = y: (agregarAlFinal x ys)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una cola devuelve el primer elemento de la cola. 
firstQ :: Queue a -> a
firstQ (Q ls) = if esVacia ls
                    then error "la cola está vacía"
                    else let (x:xs) = ls
                            in x
                        

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q ls) = if esVacia ls
                    then error "la cola está vacía"
                    else let (x:xs) = ls
                            in Q xs
                            
















