{-
Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:
-}
module Stack
    (Stack, emptyS, isEmptyS, push, top, pop, lenS )
where


-- 2. Implementar el tipo abstracto Stack utilizando una lista.

data Stack a = St [a] Int
    deriving Show
    {-
    Inv. representacion
        - El ultimo elemento en ingresar es el ultimo en salir
        - n la cantidad de elementos de la pila
    -}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Crea una pila vacía.
emptyS :: Stack a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada la cantidad de elementos en la pila.
--Costo: constante.
lenS :: Stack a -> Int
