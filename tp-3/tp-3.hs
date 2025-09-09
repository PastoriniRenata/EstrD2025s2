
--FUNCION AUXILIAR TRANSFORMA BOOLEANOS EN NROS
unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino   _  = 0



----- 1 TIPOS RECURSIVOS SIMPLES -----
-- 1.1
-- Celdas con bolitas
-- Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

{- 
    En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
    de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
    ser la siguiente:
-}

b1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
b2 = Bolita Rojo (Bolita Rojo (Bolita Rojo CeldaVacia))
b3 = Bolita Azul (Bolita Rojo (Bolita Azul (Bolita Azul (Bolita Azul CeldaVacia))))
b4 = CeldaVacia
-- Implementar las siguientes funciones sobre celdas:

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas   _    CeldaVacia  = 0
nroBolitas color (Bolita c b) = unoSiCeroSino (esMismoColor color c) + nroBolitas color b


esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor   _   _   = False

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color celda = Bolita color celda


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar   _    CeldaVacia   = CeldaVacia -- o habria que poner un error??
sacar color (Bolita c b)  = if esMismoColor color c 
                                        then b
                                        else Bolita c (sacar color b)


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 color c = c
ponerN n color c = Bolita color (ponerN (n-1) color c)


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
    1.2.
    Camino hacia el tesoro
    Tenemos los siguientes tipos de datos
-}
data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show


camino_1 = {-PASO NRO 0 -}Cofre [Cacharro ,Cacharro] 
           {-PASO NRO 1 -}     (Cofre [Cacharro] 
           {-PASO NRO 2 -}         (Nada (
           {-PASO NRO 3 -}             Cofre [] (
           {-PASO NRO 4 -}                 Nada (
           {-PASO NRO 5 -}                     Cofre [Cacharro,Cacharro,Cacharro, Tesoro] 
           {-PASO NRO 6 -}                           Fin)))))

camino_2 = {-PASO NRO 0 -}Cofre [Cacharro ,Cacharro] 
           {-PASO NRO 1 -}     (Cofre [Cacharro] 
           {-PASO NRO 2 -}         (Nada 
           {-PASO NRO 3 -}             (Cofre [] 
           {-PASO NRO 4 -}                 (Nada 
           {-PASO NRO 5 -}                     (Cofre [Cacharro,Cacharro,Cacharro] 
           {-PASO NRO 6 -}                           Fin)))))

camino_3 = {-PASO NRO 0 -}Cofre [Cacharro ,Cacharro] 
           {-PASO NRO 1 -}     (Cofre [Cacharro] 
           {-PASO NRO 2 -}         (Nada 
           {-PASO NRO 3 -}             (Cofre [Tesoro] 
           {-PASO NRO 4 -}                 (Nada 
           {-PASO NRO 5 -}                     (Cofre [Cacharro,Cacharro,Cacharro] 
           {-PASO NRO 6 -}                          Fin)))))

camino_4 =  Cofre [Cacharro ,Cacharro, Tesoro] 
               (Cofre [Cacharro] 
                   (Nada 
                      (Cofre [Tesoro,Tesoro] 
                           (Nada 
                               (Cofre [Cacharro,Cacharro,Cacharro,Tesoro] 
                                   Fin)))))

{-
hayTesoro camino_1 --> True
hayTesoro camino_2 --> False
hayTesoro camino_3 --> True

-}
-- Definir las siguientes funciones:

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro      Fin      = False
hayTesoro (Cofre obs c) = contieneTesoro obs || hayTesoro c
hayTesoro (Nada c)      = hayTesoro c


--FUNCIONES AUXILIARES 
contieneTesoro :: [Objeto] -> Bool
--PROPOSITO: Indica si en la lista de Objetos dada hay al menos 1 tesoro
contieneTesoro   []   = False
contieneTesoro (o:os) = esTesoro o || contieneTesoro os

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True
esTesoro    _   = False

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
pasosHastaTesoro camino_1 --> 5
pasosHastaTesoro camino_2 --> NO SE PUEDE EJECUT
pasosHastaTesoro camino_3 --> 3

-}
-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro      Fin      = error "No hay tesoro en el camino"
pasosHastaTesoro (Cofre obs c) = if contieneTesoro obs
                                        then 0
                                        else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)      = 1 + pasosHastaTesoro c


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0       c       =  hayTesoroEnPasoActual c
hayTesoroEn n      Fin      =  False
hayTesoroEn n (Cofre obs c) =  hayTesoroEn (n-1) c
hayTesoroEn n (Nada c)      =  hayTesoroEn (n-1) c


--FUNCION AUXILIAR
hayTesoroEnPasoActual :: Camino -> Bool
hayTesoroEnPasoActual      Fin      = False
hayTesoroEnPasoActual (Cofre obs c) = contieneTesoro obs
hayTesoroEnPasoActual (Nada c)      = False


{-
hayTesoroEn 5 camino_1 --> True
hayTesoroEn 6 camino_2 -- es el FINAL del camino-> False -->es el final del camino
hayTesoroEn 4 camino_3 -- no hay NADA --> False
hayTesoroEn 3 camino_3 --> True
-}

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Indica si hay al menos ?n? tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0       _       = True
alMenosNTesoros n      Fin      = False
alMenosNTesoros n (Cofre obs c) = n <= cantTesorosAca obs || alMenosNTesoros (n-cantTesorosAca obs) c
alMenosNTesoros n (Nada c) = alMenosNTesoros n c


-- FUNCIONES AUXILIARES
cantTesorosAca :: [Objeto] -> Int
cantTesorosAca   []   = 0
cantTesorosAca (o:os) = unoSiCeroSino (esTesoro o) + cantTesorosAca os

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--- DESAFIO ---
-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre x y  c  = if x == 0 
                                then cantDeTesorosHasta y c
                                else cantTesorosEntre (x-1) (y-1) (siguientePaso c)

-- FUNCIONES AUXILIARES
siguientePaso :: Camino -> Camino
-- Precondicion: El camino dado NO PUEDE SER `Fin`
siguientePaso     Fin       = error"ESTAS AL FINAL DEL CAMINO"
siguientePaso (Cofre obs c) = c
siguientePaso (Nada c)      = c

cantDeTesorosHasta :: Int -> Camino -> Int
cantDeTesorosHasta 0 c   = cantTesorosEnPasoActual c
cantDeTesorosHasta n Fin = 0
cantDeTesorosHasta n  c  = cantTesorosEnPasoActual c + cantDeTesorosHasta (n-1) (siguientePaso c) 

cantTesorosEnPasoActual :: Camino -> Int
-- Precondicion: El camino dado NO PUEDE SER `Fin`
cantTesorosEnPasoActual     Fin       = error"ESTAS AL FINAL DEL CAMINO" -- o pongo 0
cantTesorosEnPasoActual (Cofre obs c) = cantTesorosAca obs
cantTesorosEnPasoActual (Nada c)      = 0

{-
camino_4 = 
          paso 0: Cofre [Cacharro ,Cacharro, Tesoro] 
          paso 1    (Cofre [Cacharro] 
          paso 2        (Nada 
-Desde -> paso 3           (Cofre [Tesoro,Tesoro] 
          paso 4                (Nada 
-Hasta -> paso 5                    (Cofre [Cacharro,Cacharro,Cacharro,Tesoro] 
          paso 6                        Fin)))))


cantTesorosEntre 3 5 camino_4 --> 3

--> hay 3 cofres

-}




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


----- 2 TIPO ARBOLES -----

{-
    2.1.
    Árboles binarios
-}

-- Dada esta definición para árboles binarios defina las siguientes funciones utilizando recursión estructural según corresponda:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

tree_1 :: Tree Int
tree_2 :: Tree Int
tree_3 :: Tree Int
tree_4 :: Tree String
tree_5 :: Tree String
tree_6 :: Tree Int
tree_7 :: Tree Int

tree_1 = NodeT 1 (NodeT 2  (NodeT 3 (EmptyT) 
                                    (NodeT 4 (EmptyT) 
                                                (NodeT 5 (NodeT 6 (EmptyT) 
                                                                  (EmptyT)) 
                                                          (EmptyT)))) 
                            (EmptyT)) 
                  (NodeT 7  (EmptyT)
                            (EmptyT))
tree_2 = EmptyT
tree_3 = NodeT 10 (NodeT 10 (EmptyT) (EmptyT)) (EmptyT)
tree_4 = NodeT "hola" (NodeT "chau" (EmptyT) (EmptyT)) (EmptyT)
tree_5 = NodeT "Hola" (NodeT "Mundo" EmptyT EmptyT) 
                      (NodeT "Hola" EmptyT EmptyT)


tree_6 = NodeT 10 (EmptyT)(EmptyT)


tree_7 = NodeT 1 (NodeT 2  (NodeT 3 (EmptyT) 
                                      (NodeT 4 (EmptyT) 
                                                (NodeT 5 (NodeT 6 (EmptyT) 
                                                                    (EmptyT)) 
                                                          (EmptyT)))) 
                           (EmptyT)) 

                  (NodeT 7  (NodeT 8 (EmptyT) 
                                     (NodeT 9 (EmptyT) 
                                              (NodeT 10 (NodeT 11 (EmptyT) 
                                                                  (NodeT 12 (NodeT 13 EmptyT EmptyT)
                                                                            (EmptyT))) 
                                                        (EmptyT))))
                            (EmptyT))

arbol_1_prueba = (NodeT 1 (NodeT 2 (NodeT 3 (NodeT 4 (EmptyT)
                                                     (EmptyT))
                                            (NodeT 4 (EmptyT)
                                                     (EmptyT)))
                                   (NodeT 3 (NodeT 4 (EmptyT)
                                                     (EmptyT))
                                            (NodeT 4 (NodeT 5 (EmptyT)
                                                              (EmptyT))
                                                     (EmptyT))))
                          (NodeT 2 (NodeT 3 (NodeT 4 (EmptyT)
                                                     (EmptyT))
                                            (NodeT 4 (EmptyT)
                                                     (EmptyT)))
                                   (NodeT 3 (NodeT 4 (NodeT 5 (EmptyT)
                                                              (EmptyT))
                                                     (EmptyT))
                                            (NodeT 4 (NodeT 5 (EmptyT)
                                                              (EmptyT))
                                                     (EmptyT)))))

arbol_toList_Ordenado = (NodeT 0 (NodeT 5 (NodeT 3 (NodeT 2 (NodeT 1 EmptyT EmptyT) 
                                                             EmptyT)
                                                   (NodeT 4 EmptyT EmptyT))
                                          (NodeT 6 EmptyT EmptyT))

                                 (NodeT 8 (NodeT 7 EmptyT
                                                   EmptyT)
                                          (NodeT 9 EmptyT
                                                   (NodeT 10 EmptyT
                                                             EmptyT))))
                                 

{-
                                                       0
                                                     /  \
                                                    5    8
                                                   /\    /\
                                                  3  6  7  9
                                                 /\         \
                                                2  4        10
                                               /
                                              1
-}




-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT     EmptyT       = 0
sumarT (NodeT x t1 t2)  = x + sumarT t1 + sumarT t2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT     EmptyT       = 0
sizeT (NodeT x t1 t2)  = 1 + sizeT t2 + sizeT t2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT     EmptyT       = EmptyT
mapDobleT (NodeT x t1 t2)  = NodeT (x*2) (mapDobleT t1) (mapDobleT t2)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x     EmptyT       = False
perteneceT x (NodeT y t1 t2)  = x==y || perteneceT x t1 || perteneceT x t2


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x     EmptyT       = 0
aparicionesT x (NodeT y t1 t2)  = unoSiCeroSino (x==y) + aparicionesT x t1 + aparicionesT x t2


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: en este tipo se de?ne como hoja a un nodo con dos hijos vacíos
leaves :: Tree a -> [a]
leaves     EmptyT       = []
leaves (NodeT y t1 t2)  = if esEmpty t1 && esEmpty t2
                                then [y]
                                else leaves t1 ++ leaves t2


esEmpty :: Tree a -> Bool
esEmpty EmptyT = True
esEmpty    _   = False


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT     EmptyT       = 0
heightT (NodeT y t1 t2)  = 1 + max (heightT t1) (heightT t2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT     EmptyT       = EmptyT
mirrorT (NodeT y t1 t2)  = NodeT y (mirrorT t2) (mirrorT t1)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList     EmptyT       = []
toList (NodeT y t1 t2)  = toList t1 ++ y :  toList t2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y 
-- la distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN 0        t         = elementoActual t
levelN n      EmptyT      = []
levelN n (NodeT y t1 t2)  = levelN (n-1) t1 ++ levelN (n-1) t2

elementoActual :: Tree a -> [a]
-- proposito: devuelve el elemento del nivel actual en una lista, en caso de ser empty, devuelve una lista vacía
elementoActual     EmptyT       = []
elementoActual (NodeT y t1 t2)  = [y]

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel      EmptyT      = []
listPerLevel (NodeT y t1 t2)  = [y] : unirNivelesAnteriores (listPerLevel t1) (listPerLevel t2)


unirNivelesAnteriores :: [[a]] -> [[a]] -> [[a]]
unirNivelesAnteriores   []     ys   = ys
unirNivelesAnteriores   xs     []   = xs
unirNivelesAnteriores (x:xs) (y:ys) = (x++y): unirNivelesAnteriores xs ys


--ESTRATEGIA:
--Cuando la recursion "vuelve" Unifico la primera lista de cada rama





{-
listPerLevel tree_1 --> [[1],[2,7],[3],[4],[5],[6]] 
         rta consola--> [[1],[2,7],[3],[4],[5],[6]]

listPerLevel tree_7 --> [[1],[2,7],[3,8],[4,9],[5,10],[6,11],[12],[13]]
        rta consola--> [[1],[2,7],[3,8],[4,9],[5,10],[6,11],[12],[13]]

listPerLevel arbol_1_prueba --> [[1],[2,2],[3,3,3,3],[4,4,4,4,4,4,4,4],[5,5,5]]
                 rta consola--> [[1],[2,2],[3,3,3,3],[4,4,4,4,4,4,4,4],[5,5,5]]

listPerLevel arbol_toList_Ordenado --> [[0],[5,8],[3,6,7,9],[2,5,20],[1]]
                       rta consola --> [[0],[5,8],[3,6,7,9],[2,4,10],[1]]

-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga      EmptyT     = []
ramaMasLarga (NodeT x t1 t2) = x : laMasLarga (ramaMasLarga t1) (ramaMasLarga t2)


laMasLarga :: [a] -> [a] -> [a]
-- Devuelve la lista más larga de las 2 listas dadas
laMasLarga xs ys = if longitud xs > longitud ys 
                        then xs
                        else ys

longitud :: [a] -> Int
longitud []       = 0
longitud (_ : xs) = 1 + longitud xs



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
    Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
    ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja), o sea, por ejemplo:
            todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
                                                EmptyT)
                                     (NodeT 4 (NodeT 5 EmptyT EmptyT)
                                                EmptyT))
            
            = [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
    OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
    ésta para devolver solamente los caminos maximales.
-}
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos     EmptyT       = []
todosLosCaminos (NodeT y t1 t2)  = agregarElementoEnCadaLista y (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarElementoEnCadaLista :: a -> [[a]] -> [[a]]
agregarElementoEnCadaLista x   []   = [[x]]
agregarElementoEnCadaLista x (l:ls) = (x:l) : agregarElementoEnCadaLista x ls




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
    2.2.Expresiones Aritméticas
-}
-- El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA

-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor n )        = n 
eval (Sum   exp1 exp2) = eval exp1 + eval exp2
eval (Prod  exp1 exp2) = eval exp1 * eval exp2
eval (Neg   exp1)      = - eval exp1

nro1 = Valor 1
nro2 = Valor 2
nro3 = Sum nro1 nro2   -- 1 + 2 = 3
nro4 = Prod nro2 nro3  -- 2 * 3 = 6
nro5 = Neg nro4        -- = -6 

-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x


nro6  = Neg nro5 -- = 6
nro7  = Neg nro6 -- = -6
nro8  = Prod nro4 nro7 -- 6 * -6 = -36
nro9  = Sum nro8 nro4 -- -36 + 6 = -30
nro10 = Sum nro9 (Neg nro8) -- -30 + -(-36) = -30 + 36 = 6

nro11 = Valor 1                     -- >  1              ---- simplificar --->                                     --rta--> Valor 1                      
nro12 = Valor 0                     -- >  0              ---- simplificar --->                                     --rta--> Valor 0             
nro13 = Sum nro11 nro12             -- >  1              ---- simplificar ---> nro11 + nro12 = 1 + 0 = 1 = nro11   --rta--> Valor 1  
nro14 = Prod nro12 nro13            -- >  0              ---- simplificar ---> Prod nro12 nro13 = 0 * 1            --rta--> Valor 0               
nro15 = Sum nro13 nro14             -- >  1              ---- simplificar ---> Sum nro13 nro14 = 1 + 0 = nro13     --rta--> Valor 1
nro16 = Neg (Neg nro15)             -- > -(-1) = 1       ---- simplificar ---> Neg(Neg nro15)                      --rta--> Valor 1


nro17 = Valor 2                        -- >  2              ---- simplificar --rta--> Valor 2                      
nro18 = Valor 3                        -- >  3              ---- simplificar --rta--> Valor 3             
nro19 = Sum nro17 nro18                -- >  5              ---- simplificar --rta--> Sum (Valor 2) (Valor 3)
nro20 = Prod nro18 nro19               -- >  15             ---- simplificar --rta--> Prod (Valor 3) (Sum (Valor 2) (Valor 3))         
nro21 = Sum nro19 nro20                -- >  20             ---- simplificar --rta--> Sum (Sum (Valor 2) (Valor 3)) (Prod (Valor 3) (Sum (Valor 2) (Valor 3)))
nro22 = Neg (Neg nro21)                -- > -(-20) = 20     ---- simplificar --rta--> Sum    (Sum (Valor 2) (Valor 3))     (Prod (Valor 3) (Sum (Valor 2) (Valor 3)))
nro23 = Neg (Neg (Neg (Valor 2)))      -- > - (-(-2)) = -2 ---- simplificar --rta--> Neg (Valor 2)
nro24 = Neg (Prod (Neg nro18) nro19 )  -- >  -(- 15) = 15   ---- simplificar --rta--> Neg (Prod (Neg (Valor 3)) (Sum (Valor 2) (Valor 3))) ???      
nro25 = Sum (Sum (Valor 2) (Valor 3))  (Prod (Valor 0) (Sum (Valor 2) (Valor 3))) -- >  = 5      ---- simplificar --rta--> Sum (Valor 2) (Valor 3))
                                                                    

simplificar :: ExpA -> ExpA
simplificar (Sum exp1 exp2)   = simplificarSuma (simplificar exp1) (simplificar exp2)
simplificar (Prod exp1 exp2)  = simplificarProducto (simplificar exp1) (simplificar exp2)
simplificar (Neg exp)         = simplificarNeg (simplificar exp)
simplificar exp               = exp

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) exp2 = exp2
simplificarSuma exp1 (Valor 0) = exp1
simplificarSuma exp1 exp2      = Sum exp1 exp2

simplificarProducto :: ExpA -> ExpA -> ExpA
simplificarProducto (Valor 0) _    = Valor 0
simplificarProducto _ (Valor 0)    = Valor 0
simplificarProducto (Valor 1) exp2 = exp2
simplificarProducto exp1 (Valor 1) = exp1
simplificarProducto exp1 exp2      = Prod exp1 exp2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg exp) = exp
simplificarNeg exp = Neg exp

--EJERCICIO CLASE
type Stock = [(String, Int)]
-- OBS: no hay dos tuplas con el mismo valor de String

-- Dada una lista de stocks los combina agrupando por nombre de producto
-- y sumando las cantidades del mismo de los diferentes stocks

stock1 = [("Arroz", 3), ("Pan", 5), ("Coca",6)]
stock2 = [("Arroz", 1), ("Coca",10)]
stock3 = [("Arroz", 1), ("Pan", 1), ("Papas", 4)]


combinar :: [Stock] -> Stock
combinar   []    = []-- creo un stock inicial, que es una lista vacía
combinar (s: ss) = unificarStocks s (combinar ss) -- con la recursion yo se que trabajo con dos stocks

unificarStocks :: Stock -> Stock -> Stock
unificarStocks   []   s = s
unificarStocks (e:es) s = ingresarElemento e (unificarStocks es s)

ingresarElemento:: (String, Int) -> Stock -> Stock
ingresarElemento x   []   = [x]
ingresarElemento x (e:es) = if sonMismoProducto x e 
                                    then unificarElementos x e : es
                                    else e: ingresarElemento x es
    
unificarElementos :: (String, Int) -> (String, Int) -> (String, Int) 
unificarElementos p1 p2 = (fst p1, snd p1 + snd p2)

sonMismoProducto :: (String, Int) -> (String, Int) -> Bool
sonMismoProducto p1 p2 = fst p1 == fst p2               



{-

combinar [stock1, stock2, stock3] -->  [("Arroz", 5), ("Pan", 6), ("Coca",16), ("Papas", 4)]

-}
