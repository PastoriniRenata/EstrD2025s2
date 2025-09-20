import Data.Text.Internal.Fusion.Size (lowerBound)
-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
data Pizza = Prepizza
            | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa
                   | Queso
                   | Jamon
                   | Aceitunas Int
    deriving Show


-- EJEMPLOS
pizza1 = Capa Queso(Capa Jamon(Capa (Aceitunas 8)(Capa Salsa(Capa Jamon(Capa Queso(Capa Queso(Capa Queso (Capa Jamon Prepizza))))))))
pizza2 = Capa Jamon Prepizza
pizza3 = Capa Salsa(Capa Jamon Prepizza)
pizza4 = Capa Salsa(Capa Queso (Capa Salsa(Capa Queso Prepizza)))
prepizza = Prepizza
soloQueso = Capa Queso(Capa Queso (Capa Queso(Capa Queso Prepizza)))
soloSalsa = Capa Salsa(Capa Salsa (Capa Salsa(Capa Salsa Prepizza)))

--Denir las siguientes funciones:


--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 +  cantidadDeCapas p

{-
cantidadDeCapas pizza1 --> 9
cantidadDeCapas pizza2 --> 1
cantidadDeCapas pizza3 --> 2
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza   []   = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

{-
armarPizza [] --> Prepizza
armarPizza [Salsa,Queso,Queso,Jamon,Queso,Queso,Jamon,(Aceitunas 8)] --> Capa Salsa (Capa Queso (Capa Queso (Capa Jamon (Capa Queso (Capa Queso (Capa Jamon (Capa (Aceituna 8) Prepizza)))))))
armarPizza [Jamon] -> Capa Jamon Prepizza
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon  Prepizza  = Prepizza
sacarJamon (Capa i p) = if esJamon i
                            then sacarJamon p
                            else Capa i (sacarJamon p)


esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon   _   = False

{-
pizza1 = Capa Queso(Capa Jamon(Capa (Aceitunas 8)(Capa Salsa(Capa Jamon(Capa Queso(Capa Queso(Capa Queso (Capa Jamon Prepizza))))))))
sacarJamon pizza1 --> Capa Queso (Capa (Aceitunas 8) (Capa Salsa (Capa Queso (Capa Queso (Capa Queso Prepizza)))))
                      Capa Queso (Capa (Aceitunas 8) (Capa Salsa (Capa Queso (Capa Queso (Capa Queso Prepizza)))))

pizza2 = Capa Jamon Prepizza
sacarJamon pizza2 --> Prepizza


pizza3 = Capa Queso(Capa Jamon Prepizza)
sacarJamon pizza3 --> Capa Salsa Prepizza

-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso p = let (s,q,o) = recuentoIngredientes p
                            in s>0 && q>0 && o==0
                        


recuentoIngredientes :: Pizza -> (Int, Int, Int)
recuentoIngredientes   Prepizza     = (0,0,0)
recuentoIngredientes (Capa Salsa p) = sumarTuplas (1,0,0) (recuentoIngredientes p)
recuentoIngredientes (Capa Queso p) = sumarTuplas (0,1,0) (recuentoIngredientes p)
recuentoIngredientes (Capa   _   p) = sumarTuplas (0,0,1) (recuentoIngredientes p)


sumarTuplas :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumarTuplas (a,b,c) (x,y,z) = (a+x, b+y, c+z)



{-
tieneSoloSalsaYQueso pizza1 --> False
tieneSoloSalsaYQueso pizza2 --> False 
tieneSoloSalsaYQueso pizza3 --> False
tieneSoloSalsaYQueso pizza4 --> True

tieneSoloSalsaYQueso prepizza  --> False
tieneSoloSalsaYQueso soloQueso --> False
tieneSoloSalsaYQueso soloSalsa --> False
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas  Prepizza  = Prepizza
duplicarAceitunas (Capa i c) = if esAceituna i
                                    then Capa (duplicarLaCantidad i) (duplicarAceitunas c)
                                    else Capa i (duplicarAceitunas c)


--FUNCION AUXILIAR
esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _ ) = True
esAceituna      _        = False


duplicarLaCantidad :: Ingrediente -> Ingrediente
-- Proposito: Dado un ingrediente de tipo aceituna, devuelve el mismo duplicando su cantidad
-- Precondicion: el ingrediente dado debe ser de tipo Aceituna
duplicarLaCantidad (Aceitunas n) = Aceitunas (n*2)
duplicarLaCantidad      _       = error"NO SON ACEITUNAS"



{-
duplicarAceitunas pizza1 --> Capa Queso (Capa Jamon (Capa (Aceitunas 16) (Capa Salsa (Capa Jamon (Capa Queso (Capa Queso (Capa Queso (Capa Jamon Prepizza))))))))
duplicarAceitunas pizza2 --> Capa Jamon Prepizza
duplicarAceitunas pizza3 --> Capa Salsa(Capa Jamon Prepizza)
duplicarAceitunas pizza4 --> Capa Salsa(Capa Queso (Capa Salsa(Capa Queso Prepizza)))

-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza   []   = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

{-
cantCapasPorPizza [pizza1, pizza2, pizza3, pizza4] --> [(9 , Capa Queso(Capa Jamon(Capa (Aceitunas 8)(Capa Salsa(Capa Jamon(Capa Queso(Capa Queso(Capa Queso (Capa Jamon Prepizza))))))))),
                                                        (1 , Capa Jamon Prepizza),
                                                        (2 , Capa Salsa(Capa Jamon Prepizza)),
                                                        (4 , Capa Salsa(Capa Queso (Capa Salsa(Capa Queso Prepizza))))]
                                                       
                                                       

-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--2. Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
    deriving Show


--FUNCIONES OBSERVADORAS
objetosDel :: Cofre -> [Objeto]
objetosDel (Cofre objs) = objs


cofreActual :: Mapa -> Cofre
cofreActual       (Fin c )       = c          
cofreActual (Bifurcacion c _ _ ) = c                


mapa_1 = (Bifurcacion (Cofre [Chatarra, Chatarra,Chatarra]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra]))
                                                                                                      (Fin (Cofre [Chatarra, Tesoro])))      
                                                            (Fin (Cofre [Chatarra, Tesoro])))


{-
                                                Cofre [Chatarra, Chatarra,Chatarra]
                                                 /                             \
                                                /                               \
                            Cofre [Chatarra, Chatarra]                        Cofre [Chatarra, Tesoro]
                                     /             \                                   
                                    /               \                                  
                        Cofre [Chatarra]         Cofre [Chatarra, Tesoro] 
-} 
mapa_2 = (Bifurcacion (Cofre [Chatarra, Chatarra,Chatarra]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra]))
                                                                                                      (Fin (Cofre [Chatarra])))      
                                                            (Fin (Cofre [Chatarra])))
{-NO TIENE TESOROS
                                                Cofre [Chatarra, Chatarra,Chatarra]
                                                 /                             \
                                                /                               \
                            Cofre [Chatarra, Chatarra]                        Cofre [Chatarra]
                                     /             \                                   
                                    /               \                                  
                        Cofre [Chatarra]         Cofre [Chatarra] 
-}
mapa_3 = (Bifurcacion (Cofre [Chatarra, Chatarra,Tesoro,Chatarra]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra]))
                                                                                                             (Fin (Cofre [Chatarra, Tesoro])))      
                                                                   (Fin (Cofre [Chatarra, Tesoro])))
mapa_4 = (Fin (Cofre [Chatarra, Tesoro]))
mapa_5 = (Fin (Cofre []))
mapa_6 = (Bifurcacion (Cofre [Chatarra, Chatarra,Chatarra]) (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra]))
                                                                                                      (Fin (Cofre [Chatarra, Tesoro])))      
                                                            (Fin (Cofre [Chatarra, Chatarra])))
{-
                                        Bifurcacion Cofre [Chatarra, Chatarra,Chatarra]
                                                 /                             \
                                                /                               \
                         Bifurcacion Cofre [Chatarra, Chatarra]               Fin Cofre [Chatarra, Chatarra]
                                     /             \                                   
                                    /               \                                  
                     Fin Cofre [Chatarra]        Fin Cofre [Chatarra, Tesoro] 
caminoAlTesoro mapa_6 --> [Izq, Der]
-} 
mapa_7 = Bifurcacion (Cofre []) (Fin (Cofre [])) (Fin (Cofre []))
mapa_8 = (Bifurcacion (Cofre [Chatarra, Chatarra,Tesoro,Chatarra]) (Fin (Cofre [Chatarra, Tesoro]))
                                                                   (Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra]))
                                                                                                             (Fin (Cofre [Chatarra, Tesoro]))) )     
                                              

--Denir las siguientes operaciones:
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro      (Fin c)          = existeTesoro c
hayTesoro (Bifurcacion c m1 m2) = existeTesoro c || hayTesoro m1 || hayTesoro m2


--FUNCIONES AUXILIARES
existeTesoro :: Cofre -> Bool
existeTesoro c = hayTesoroEnList (objetosDel c)


hayTesoroEnList :: [Objeto] -> Bool
hayTesoroEnList   []   = False
hayTesoroEnList (o:os) = esTesoro o || hayTesoroEnList os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro    _   = False

{-
hayTesoro mapa_1 --> True
hayTesoro mapa_2 --> False
hayTesoro mapa_3 --> True
hayTesoro mapa_4 --> True
hayTesoro mapa_5 --> False
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn   []    m = existeTesoro (cofreActual m)
hayTesoroEn (d:ds)  m = if esFin m
                            then error "No llegaste al final del camino, te quedan posiciones por recorrer"
                            else hayTesoroEn ds (siguientePaso d m)


esFin :: Mapa -> Bool
esFin (Fin _) = True
esFin   _     = False

siguientePaso :: Dir -> Mapa -> Mapa
--Precondicion: el mapa es una bifurcacion
siguientePaso Izq (Bifurcacion _ m1 _) = m1
siguientePaso Der (Bifurcacion _ _ m2) = m2


{-
hayTesoroEn [Izq,Der] mapa_1            --> True
hayTesoroEn [Der] mapa_1                --> True
hayTesoroEn [Izq, Izq, Izq, Izq] mapa_2 --> Error -> EL MAPA LLEGA A FIN ALTES DE QUE SE TERMINE LA LISTA
hayTesoroEn [] mapa_3                   --> True
hayTesoroEn [Der, Der] mapa_4           --> Error 
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro mapa = if existeTesoro (cofreActual mapa) --Si el tesoro esta en el paso actual, devuelve la lista vacía
                            then []
                            else let direccion = direccionAlTesoro mapa
                                 in
                                    direccion : caminoAlTesoro (mapaEnDireccion direccion mapa)


--FUNCIONES AUXILIARES
direccionAlTesoro :: Mapa -> Dir
-- Dado un mapa, describe en que direccion de sus bifurcaciones hay un tesoro
-- PRECONDICION: existe un tesoro y es único
direccionAlTesoro      (Fin _ )          = error "NO HAY TESORO EN EL MAPA ACTUAL" -- si hubiera tesoro en el Fin, debería haber entrado en la linea 270 en el then
direccionAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1
                                                then Izq
                                                else Der

mapaEnDireccion :: Dir -> Mapa -> Mapa
--Proposito: dada una direccion y un mapa, devuelve el mapa en la direccion dada
-- PRECONDICION: Existe un siguiente mapa (osea q no es fin)
mapaEnDireccion Izq (Bifurcacion c m1 _) = m1
mapaEnDireccion Der (Bifurcacion c _ m2) = m2
mapaEnDireccion  _  (Fin _ )             = error"NO HAY SIGUIENTE MAPA"


{-

caminoAlTesoro mapa_4 --> [] // xq está en el primer paso que 
caminoAlTesoro mapa_5 --> ERROR, no hay tesoro
caminoAlTesoro mapa_6 --> [Izq, Der]

-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga      (Fin _ )         = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if elMapaEsMasLargo m1 m2
                                                         then Izq : caminoDeLaRamaMasLarga m1
                                                         else Der : caminoDeLaRamaMasLarga m2
                                                            
                                                            

elMapaEsMasLargo :: Mapa -> Mapa -> Bool
-- Dados dos mapas, indica si el primer mapa es mas largo que el segundo
elMapaEsMasLargo m1 m2 = longitudMapa m1 > longitudMapa m2

longitudMapa :: Mapa -> Int
longitudMapa     (Fin _)            = 0
longitudMapa (Bifurcacion _ m1 m2)  = 1 + max (longitudMapa m1) (longitudMapa m2)


{-
caminoDeLaRamaMasLarga mapa_1 --> [Izq, Der]
caminoDeLaRamaMasLarga mapa_2 --> [Izq, Der]
caminoDeLaRamaMasLarga mapa_4 --> []
caminoDeLaRamaMasLarga mapa_7 --> [Izq]
caminoDeLaRamaMasLarga mapa_8 --> [Der, Der]
-}



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel         (Fin c)       = [losTesorosDe c]
tesorosPorNivel (Bifurcacion c m1 m2) = let
                                            (x:tesorosm1) = tesorosPorNivel m1
                                            (y:tesorosm2) = tesorosPorNivel m2
                                                in losTesorosDe c : (x++y) : tesorosm1 ++ tesorosm2

--estrategia: unifico la primera lista de cada recursion (osea los del mismo nivel)



--FUNCIONES AUXILIARES
losTesorosDe :: Cofre -> [Objeto]
losTesorosDe (Cofre objs) = losTesorosEn objs

losTesorosEn :: [Objeto] -> [Objeto]
losTesorosEn   []   = []
losTesorosEn (o:os) = if esTesoro o
                            then o : losTesorosEn os
                            else losTesorosEn os



{-
tesorosPorNivel mapa_1 --> [[],[Tesoro],[Tesoro]]
tesorosPorNivel mapa_2 --> [[],[],[]]
tesorosPorNivel mapa_6 --> [[],[],[Tesoro]]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos         (Fin _)       = []
todosLosCaminos (Bifurcacion _ m1 m2) = agregarEnTodas Izq (todosLosCaminos m1) ++ agregarEnTodas Der (todosLosCaminos m2) 

--FUNCIONES AUXILIARES
agregarEnTodas :: a -> [[a]] -> [[a]]
agregarEnTodas x  []   = [[x]]
agregarEnTodas x (l:ls) = (x:l) : agregarEnTodas x ls


{-
todosLosCaminos mapa_1 --> [[Izq,Izq],[Izq,Der],[Der],[Izq]]
todosLosCaminos mapa_4 --> []
todosLosCaminos mapa_7 --> [[Izq],[Der]]
todosLosCaminos mapa_8 --> [[Izq],[Der],[Der,Izq],[Der,Der]]
-}


--VERSION CAMINOS MAXIMALES
todosLosCaminosMaximales :: Mapa -> [[Dir]]
todosLosCaminosMaximales         (Fin _)       = [[]]
todosLosCaminosMaximales (Bifurcacion _ m1 m2) = agregarElementoAdelanteDeTodas Izq (todosLosCaminosMaximales m1) ++ agregarElementoAdelanteDeTodas Der (todosLosCaminosMaximales m2) 

--FUNCIONES AUXILIARES
agregarElementoAdelanteDeTodas :: a -> [[a]] -> [[a]]
agregarElementoAdelanteDeTodas _   []   = []
agregarElementoAdelanteDeTodas x (l:ls) = (x:l) : agregarElementoAdelanteDeTodas x ls

{-
todosLosCaminosMaximales mapa_1 --> [[Izq,Izq],[Izq,Der],[Der]]
todosLosCaminosMaximales mapa_4 --> [[]]
todosLosCaminosMaximales mapa_7 --> [[Izq],[Der]]
todosLosCaminosMaximales mapa_8 --> [[Izq],[Der,Izq],[Der,Der]]
-}


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Nave Espacial
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show


--FUNCIONES OBSERVADORAS
idDelSector :: Sector -> SectorId
idDelSector (S id _ _) = id

losTripDelSector :: Sector -> [Tripulante]
losTripDelSector (S _ _ tps) = tps




nave_1 = N ( NodeT (S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  (NodeT  (S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"]) EmptyT 
                                                                                                                                      EmptyT   ) 

                                                                          (NodeT  (S "C" [ (Motor 30)] ["Pabli", "Ami"]) EmptyT
                                                                                                                         (NodeT (S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar"]) EmptyT
                                                                                                                                                                                                     EmptyT )))
{-
                                                S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  
                                                /                                               \
                                               /                                                 \
        S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"])                                      S "C" [ (Motor 30)] ["Pabli", "Ami"]
                                                                                                                           \
                                                                                                                            \
                                                                                                                        S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar"]
-}
nave_2 = N ( NodeT (S "E" [LanzaTorpedos, (Motor 1)] [ "Gonza"])  (NodeT  (S "F" [LanzaTorpedos, (Motor 2), Almacen [Oxigeno, Combustible] ] ["Azu"]) EmptyT 
                                                                                                                     EmptyT) 

                                                                  (NodeT  (S "G" [ (Motor 3)] ["Pabli", "Ami"]) EmptyT
                                                                                                                (NodeT (S "H" [Almacen [Comida, Torpedo, Combustible]] ["Mile", "Ami"]) EmptyT
                                                                                                                                                                                 EmptyT )))
nave_3 = N ( NodeT (S "A" [LanzaTorpedos, (Motor 10)] ["Rena", "Gonza"])  (NodeT  (S "B" [LanzaTorpedos, (Motor 20)] ["Azu", "Mile"]) EmptyT 
                                                                                                                                      EmptyT   ) 

                                                                          (NodeT  (S "C" [ (Motor 30)] ["Pabli","Mile","Ami"]) EmptyT
                                                                                                                         (NodeT (S "D" [(Motor 40), Almacen [Comida, Oxigeno, Combustible]] ["Mar","Rena"]) EmptyT
                                                                                                                                                                                                     EmptyT )))

--Implementar las siguientes funciones utilizando recursión estructural:
--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N sts) = losSectores sts


losSectores :: Tree Sector -> [SectorId]
losSectores       EmptyT    = []
losSectores (NodeT s t1 t2) = (idDelSector s) : losSectores t1 ++ losSectores t2

{-
sectores nave_1 --> ["A", "B", "C", "D"]
sectores nave_2 --> ["E", "F", "G", "H"]
sectores nave_3 --> ["A", "B", "C", "D"]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sts) = propulsionTotal sts


--FUNCIONES AUXILIARES
propulsionTotal :: Tree Sector -> Int
--Proposito: Devuelve la suma de poder de propulsión de todos los motores del tree de sectores.
propulsionTotal       EmptyT    = 0
propulsionTotal (NodeT s t1 t2) = propulsion s + propulsionTotal t1 + propulsionTotal t2

propulsion :: Sector -> Int
--Proposito: Devuelve la suma de poder de propulsión del sector dado.
propulsion (S _ comps _ ) = poderMotores comps

poderMotores :: [Componente] -> Int
--Proposito: Devuelve la suma de poder de propulsión de la lista de componentes dada.
poderMotores   []   = 0
poderMotores (c:cs) = poderProp c + poderMotores cs

poderProp :: Componente -> Int
--Proposito: Devuelve el poder de propulsion del componente dado, en caso de no ser un motor devuelve 0.
poderProp (Motor p) = p
poderProp     _     = 0


{-
poderDePropulsion nave_1  ---> 100
poderDePropulsion nave_2  ---> 6
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N scts) = losBarrilesDelTree scts

losBarrilesDelTree :: Tree Sector -> [Barril]
-- Proposito: devuelve los barriles de todos los sectores del tree dado
losBarrilesDelTree       EmptyT    = []
losBarrilesDelTree (NodeT s t1 t2) = losBarrilesDelSector s ++ losBarrilesDelTree t1 ++ losBarrilesDelTree t2

losBarrilesDelSector :: Sector -> [Barril]
-- Proposito: devuelve los barriles del sector dado
losBarrilesDelSector (S _ comps _ ) = losBarrilesEn comps

losBarrilesEn :: [Componente] -> [Barril]
-- Proposito: devuelve los barriles de la lista de componentes dada
losBarrilesEn   []   = []
losBarrilesEn (c:cs) = barrilesEn c ++ losBarrilesEn cs

barrilesEn :: Componente -> [Barril]
-- Proposito: devuelve los barriles del componente dado, en caso de no ser un almacen, devuelve la lista vacía
barrilesEn (Almacen barriles) = barriles
barrilesEn          _         = []



{-
barriles nave_1 --> [Comida, Oxigeno, Combustible]
barriles nave_2 --> [Oxigeno, Combustible, Comida, Torpedo, Combustible]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N sects) = (N (insertarEnSector cs id sects))


--FUNCIONES AUXILIARES
insertarEnSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
--Propósito: Añade una lista de componentes a un sector del Tree de sectores.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
insertarEnSector  _  _     EmptyT      = EmptyT 
insertarEnSector cs id (NodeT s s1 s2) = if esMismoId id (idDelSector s)
                                                then NodeT (agregarComponentes cs s) s1 s2
                                                else NodeT s (insertarEnSector cs id s1) (insertarEnSector cs id s2)

esMismoId :: SectorId -> SectorId -> Bool
--Indica si es el mismo id 
esMismoId id1 id2 = id1 == id2

agregarComponentes :: [Componente] -> Sector -> Sector
-- Proposito: agrega la lista de componentes dada al sector dado
agregarComponentes cs1 (S id cs2 tps) = S id (cs2++cs1) tps

{-
agregarASector [LanzaTorpedos, Motor 22, Almacen [Oxigeno]] "C" nave_1 --> 
nave_1 = N ( NodeT (S "A" [LanzaTorpedos, Motor 10] ["Rena", "Gonza"]) (NodeT  (S "B" [LanzaTorpedos, Motor 20] ["Azu", "Mile"]) EmptyT 
                                                                                                                                 EmptyT   ) 

                                                                          (NodeT  (S "C" [ (Motor 30), LanzaTorpedos, Motor 22, Almacen [Oxigeno]] ["Pabli", "Ami"]) EmptyT

agregarASector [LanzaTorpedos, Motor 22, Almacen [Oxigeno]] "H" nave_2 --> 
nave_2 = N ( NodeT (S "E" [LanzaTorpedos, (Motor 1)] [ "Gonza"])  (NodeT  (S "F" [LanzaTorpedos, (Motor 2), Almacen [Oxigeno, Combustible] ] ["Azu"]) EmptyT 
                                                                                                                     EmptyT) 

                                                                  (NodeT  (S "G" [ (Motor 3)] ["Pabli", "Ami"]) EmptyT
                                                                                                                (NodeT (S "H" [Almacen [Comida, Torpedo, Combustible], LanzaTorpedos, Motor 22, Almacen [Oxigeno]] ["Mile"]) EmptyT
        
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ids (N sectores) = N (asignarTripA t ids sectores)

--FUNCIONES AUXILAIRES
asignarTripA :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
--Proposito: Dados un tripulante, una lista de ids de sectores y un Tree de sectores, asigna el tripulante a 
-- cada sector en la lista de sectores
--Precondición: Todos los id de la lista existen en el Tree.
asignarTripA _    []    sectores = sectores
asignarTripA t (id:ids) sectores = asignarEn id t (asignarTripA t ids sectores)

asignarEn :: SectorId -> Tripulante ->  Tree Sector -> Tree Sector
--Proposito: asigna al tripulante dado en el sector del Tree con id dado
--Precondición: El id existe en el Tree.
asignarEn id t         EmptyT  = EmptyT
asignarEn id t (NodeT s s1 s2) = if esMismoId id (idDelSector s)
                                      then NodeT (agregarTripulante t s) s1 s2
                                      else NodeT s (asignarEn id t s1) (asignarEn id t s2)


agregarTripulante :: Tripulante -> Sector -> Sector
--Proposito: agregar el tripulante a la lista de tripulantes del sector dado
agregarTripulante t (S id cps tps) = S id cps (t:tps)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N sects) = losSectoresAsignados t sects

--FUNCIONES AUXILIARES
losSectoresAsignados :: Tripulante -> Tree Sector -> [SectorId]
--
losSectoresAsignados t      EmptyT     = []
losSectoresAsignados t (NodeT s s1 s2) = sectorSiEstaAsignado t s ++ losSectoresAsignados t s1 ++ losSectoresAsignados t s2

sectorSiEstaAsignado :: Tripulante -> Sector -> [SectorId]
sectorSiEstaAsignado t (S id _ tps) = if pertenece t tps -- los tripulantes son strings
                                                then [id]
                                                else []

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece y (x:xs) = y==x || pertenece y xs


{-
sectoresAsignados "Rena" nave_1 --> ["A"]
sectoresAsignados "Rena" nave_2 --> []
sectoresAsignados "Mar" nave_2--> []
sectoresAsignados "Ami" nave_2--> ["G", "H"]
sectoresAsignados "Rena" nave_3 --> ["A", "D"]
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N sectores) = losTripulantes sectores


--FUNCIONES AUXILIARES
losTripulantes :: Tree Sector -> [Tripulante]
losTripulantes EmptyT = []
losTripulantes (NodeT s s1 s2) = 
                    let tripSect1 = losTripulantes s1
                        tripSect2 = losTripulantes s2
                    in 
                        appendSinRepetidos (losTripDelSector s) (appendSinRepetidos tripSect1 tripSect2)


appendSinRepetidos :: [Tripulante] -> [Tripulante] -> [Tripulante]
appendSinRepetidos   []   tps = tps
appendSinRepetidos (t:ts) tps = if pertenece t tps
                                    then appendSinRepetidos ts tps
                                    else t : appendSinRepetidos ts tps

{-
tripulantes nave_1 -no tiene repetidos-> ["Rena","Gonza","Azu","Mile","Pabli","Ami","Mar"]
tripulantes nave_3 -tiene repetidos   -> ["Gonza","Azu","Pabli","Mile","Ami","Mar","Rena"]
-}






--Manada de lobos
--Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
--de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
--Los diferentes casos de lobos que forman la jerarquía son los siguientes:
--Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
--Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
--bosques, ríos, etc.), y poseen 2 lobos a cargo.
--Las crías poseen sólo un nombre y no poseen lobos a cargo.
--La estructura es la siguiente:
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cría Nombre
    deriving Show

data Manada = M Lobo
    deriving Show


manada1 = M (Cazador "A" ["carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                               (Explorador "D" ["Terr3"] (Cría "E") 
                                                                                                         (Cría "F"))) 
                                             (Cría "G") 
                                             (Cazador "H" ["pescado"] (Cría "I")
                                                                      (Cría "J")
                                                                      (Cazador "K" ["pollo", "carne"] (Cría "L")
                                                                                                      (Cría "M")
                                                                                                      (Cría "N"))))
{-                                                                      Cazador "A" ["carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"              Cazador "H" ["pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr3"]                         Cría "I"     Cría "J"       Cazador "K" ["pollo", "carne"]
                                                 /                       \                                                       /          |           \
                                                /                         \                                                     /           |            \       
                                            Cría "E"                   Cría "F"                                             Cría "L"    Cría "M"       Cría "N" 
-}

manada2 = M (Cazador "A" ["carne","pescado","carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                                                 (Explorador "D" ["Terr1"] (Cría "E") 
                                                                                                                           (Cría "F"))) 
                                                               (Cría "G") 
                                                               (Cazador "H" ["pescado","carne","pescado"] (Cría "I")
                                                                                                          (Cría "J")
                                                                                                          (Cazador "K" ["pollo", "carne","carne","pescado"] (Cría "L")
                                                                                                                                                            (Cría "M")
                                                                                                                                                            (Cría "N"))))
{-                                                            Cazador "A" ["carne","pescado","carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"         Cazador "H" ["pescado","carne","pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr1"]                         Cría "I"     Cría "J"    Cazador "K" ["pollo", "carne","carne","pescado"]
                                                 /                       \                                                       /          |           \
                                                /                         \                                                     /           |            \       
                                            Cría "E"                   Cría "F"                                             Cría "L"    Cría "M"       Cría "N"   

          
-}

manada3 = M (Cazador "A" ["carne","pescado","carne","pescado"] (Explorador "B" ["Terr1","Terr2"] (Cría "C") 
                                                                                                 (Explorador "D" ["Terr3"] (Cría "E") 
                                                                                                                           (Cazador "F" ["carne","pescado","carne","pescado","carne","pescado"] (Cría "O")
                                                                                                                                                                                                (Cría "P")
                                                                                                                                                                                                (Cría "Q")))) 
                                                               (Cría "G") 
                                                               (Cazador "H" ["pescado","carne","pescado"] (Cría "I")
                                                                                                          (Cría "J")
                                                                                                          (Cazador "K" ["pollo", "carne","carne","pescado"] (Cría "L")
                                                                                                                                                            (Cría "M")
                                                                                                                                                            (Cría "N"))))
{-                                                             Cazador "A" ["carne","pescado","carne","pescado"]
                                                                       /             |                  \
                                                                      /              |                   \
                                        Explorador "B" ["Terr1","Terr2"]           Cría "G"         Cazador "H" ["pescado","carne","pescado"]
                                        /                 \                                             /          |            \
                                       /                   \                                           /           |             \
                                 Cría "C"         Explorador "D" ["Terr3"]                         Cría "I"     Cría "J"    Cazador "K" ["pollo", "carne","carne","pescado"]
                                                 /                       \                                                                   /          |           \
                                                /                         \                                                                 /           |            \       
                                          Cría "E"       Cazador "F" ["carne","pescado","carne","pescado","carne","pescado"]            Cría "L"    Cría "M"       Cría "N"                
                                                           /                  |                    \                                            
                                                          /                   |                     \                                                 
                                                      Cría "O"             Cría "P"              Cría "Q"




-}



--Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--que corresponda en cada caso:

manada = M (Cazador "Gonza" ["Pez", "Cierbo", "Conejo"] (Explorador "Azu" ["Rio", "Bosque"] (Cría "Rena") (Cría "Pablo"))
                                                       (Explorador "Ami" ["Lago"]          (Cría "Mar") (Cría "Fidel"))
                                                       (Cría "Mile" ))


--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza manada =  cantAlimentoEn manada > cantDeCríasEn manada

--FUNCIONES AUXILIARES
cantAlimentoEn :: Manada -> Int
cantAlimentoEn (M lobo) = cantAlimento lobo 

cantAlimento :: Lobo -> Int
cantAlimento (Cazador _ presas l1 l2 l3) = longitud presas + cantAlimento l1 + cantAlimento l2 + cantAlimento l3
cantAlimento   (Explorador _ _ l1 l2)    = cantAlimento l1 + cantAlimento l2
cantAlimento           (Cría _)          = 0

longitud :: [a] -> Int
longitud []       = 0
longitud (_ : xs) = 1 + longitud xs
 
cantDeCríasEn :: Manada -> Int
cantDeCríasEn (M lobo) = cantCrías lobo

cantCrías :: Lobo -> Int
cantCrías (Cazador _ _ l1 l2 l3) = cantCrías l1 + cantCrías l2 + cantCrías l3
cantCrías (Explorador _ _ l1 l2) = cantCrías l1 + cantCrías l2
cantCrías        (Cría _)        = 1

{-

buenaCaza manada  --> False ( 3 presas y  5 Crías)
buenaCaza manada1 --> False ( 5 presas y  9 Crías)
buenaCaza manada2 --> True  (11 presas y  9 Crías)
buenaCaza manada3 --> True  (17 presas y 11 Crías)
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = alfaEn lobo

alfaEn :: Lobo -> (Nombre, Int)
alfaEn (Cazador n presas l1 l2 l3) = elQueTieneMasPresas (n, longitud presas) (elQueTieneMasPresas (alfaEn l1) (elQueTieneMasPresas (alfaEn l2) (alfaEn l3)))
alfaEn  (Explorador _ _ l1 l2)     = elQueTieneMasPresas (alfaEn l1) (alfaEn l2)
alfaEn          (Cría _)           = ("",0)


elQueTieneMasPresas :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int) 
elQueTieneMasPresas (n1, c1) (n2, c2) = if c1>c2 then (n1, c1)
                                                 else (n2, c2)

{-
elAlfa manada1 --por como está programado--> ("K",2)
elAlfa manada2 --por como está programado--> ("K",4)
elAlfa manada3 ----------------------------> ("F",6)
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = losExploradoresDe t lobo


losExploradoresDe :: Territorio -> Lobo -> [Nombre]
losExploradoresDe t (Cazador _ _ l1 l2 l3)   = losExploradoresDe t l1 ++ losExploradoresDe t l2 ++ losExploradoresDe t l3
losExploradoresDe t          (Cría _)        = []
losExploradoresDe t  (Explorador n ts l1 l2) = if pertenece t ts 
                                                then n : losExploradoresDe t l1 ++ losExploradoresDe t l2
                                                else losExploradoresDe t l1 ++ losExploradoresDe t l2

{-
losQueExploraron "Terr1" manada1 --> ["B"]
losQueExploraron "Terr3" manada1 --> ["D"]
losQueExploraron "Terr2" manada1 --> ["B"]
-}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un terri-
--torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = losExploradoresPorTerritorio lobo
{-
cuando llego a un explorador, agarro su lista de territorios y por cada territorio en esa lista: veo - si el territorio ya está en la lista de tuplas, si ya está agrego el lobo(su nombre) a la lista de la tupl
                                                                                                     - si el territorio no está, creo la tupla con el nombre del lobo en el q estoy


-}

losExploradoresPorTerritorio :: Lobo -> [(Territorio, [Nombre])]
losExploradoresPorTerritorio  (Explorador n ts l1 l2) = agregarATuplas n ts (unificarTuplasSinRepetidos (losExploradoresPorTerritorio l1) 
                                                                                                        (losExploradoresPorTerritorio l2))
losExploradoresPorTerritorio (Cazador _ _ l1 l2 l3)   = unificarTuplasSinRepetidos (losExploradoresPorTerritorio l1) 
                                                                                   (unificarTuplasSinRepetidos (losExploradoresPorTerritorio l2) 
                                                                                                               (losExploradoresPorTerritorio l3))
losExploradoresPorTerritorio          (Cría _)        = []


unificarTuplasSinRepetidos :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--PROPOSITO: dadas dos listas del tipo (Territorio, [Nombre]) unifica las mismas en una sola q no contenga repetidos.
unificarTuplasSinRepetidos   []   ys = ys
unificarTuplasSinRepetidos (t:ts) ys = ingresarTupla t (unificarTuplasSinRepetidos ts ys)

ingresarTupla :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
ingresarTupla x   []   = [x]
ingresarTupla x (t:ts) = if sonMismoTerritorio (fst x) (fst t) 
                                    then unificarTupla x t : ts
                                    else t : ingresarTupla x ts

sonMismoTerritorio :: Territorio -> Territorio -> Bool
sonMismoTerritorio t1 t2 = t1==t2


unificarTupla :: (Territorio, [Nombre]) -> (Territorio, [Nombre]) -> (Territorio, [Nombre])
unificarTupla t1 t2 = (fst t1, snd t1 ++ snd t2)


agregarATuplas :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--Proposito: agregar el nombre dado a los territorios dados en la lista de (Territorio, [Nombre]) dada
agregarATuplas n   []   tss = tss
agregarATuplas n (t:ts) tss = ingresarTupla (t, [n]) (agregarATuplas n ts tss) 

{-
Cazador Nombre [Presa] Lobo Lobo Lobo
Explorador Nombre [Territorio] Lobo Lobo
Cría Nombre

exploradoresPorTerritorio manada1 --> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]   
                  rta en terminal---> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]            
exploradoresPorTerritorio manada2 --> [("Terr1",["B","D"]),("Terr2",["B"])] 
                  rta en terminal---> [("Terr2",["B"]),("Terr1",["B","D"])]  -- xq como esta programado lo de sin repetidos me queda al reves
exploradoresPorTerritorio manada3 --> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]
                  rta en terminal---> [("Terr1",["B"]),("Terr2",["B"]),("Terr3",["D"])]
-}

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los ca-
--zadores que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
--subordinado de un subordinado).
{-
cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--Precondición: hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe n (M lobo) = busquedaDeCazSuperiores n lobo 

busquedaDeCazSuperiores :: Nombre -> Lobo -> [Nombre]
busquedaDeCazSuperiores n lobo = snd (losCazadoresSuperiores n lobo)


losCazadoresSuperiores :: Nombre -> Lobo -> (Bool, [Nombre])
--PROPOSITO: dado un lobo y un nombre, en la tupla resultante indica True si en el lobo dado existe un lobo con ese nombre
-- y la lista de nombres de los cazadores que son superiores a dicho lobo
losCazadoresSuperiores n1          (Cría n2)        = (n1==n2, [])
losCazadoresSuperiores n1 (Cazador n2 _ l1 l2 l3)   = if n1 == n2 
                                                        then (True, [])
                                                        else let t1 = losCazadoresSuperiores n1 l1
                                                                 t2 = losCazadoresSuperiores n1 l2
                                                                 t3 = losCazadoresSuperiores n1 l3
                                                                 (b, ns) = laTuplaQueCumple t1 (laTuplaQueCumple t2 t3)
                                                                in (b, n2 : ns)

losCazadoresSuperiores n1  (Explorador n2 _ l1 l2)  = if n1 == n2 
                                                        then (True, [])
                                                        else let t1 = losCazadoresSuperiores n1 l1
                                                                 t2 = losCazadoresSuperiores n1 l2
                                                                in laTuplaQueCumple t1 t2


laTuplaQueCumple :: (Bool, [Nombre]) -> (Bool, [Nombre]) -> (Bool, [Nombre]) 
--Proposito: en caso de que la primera tupla tenga un true, devuelve la misma, sino devuelve la otr tupla
laTuplaQueCumple (True, ns) _ = (True, ns)
laTuplaQueCumple    _      t2 = t2

--estrategia: recolecto los lobos cazadores hasta encontrar al lobo dado.
-}


----------------------------------------------------------------------------------
--VERSION CON IFS vista en clase



cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--Precondición: hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe n (M lobo) = cazadoresSuperioresDeLobo n lobo 

cazadoresSuperioresDeLobo :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeLobo nom         (Cría n)          = if nom == n then [] else error "el lobo no está en la manada"
cazadoresSuperioresDeLobo nom (Cazador n _ l1 l2 l3)    = if nom == n then [] else if existeLoboEn nom l1
                                                                                        then n: cazadoresSuperioresDeLobo nom l1
                                                                                        else if existeLoboEn nom l2
                                                                                                then n : cazadoresSuperioresDeLobo nom l2
                                                                                                else n : cazadoresSuperioresDeLobo nom l3 -- x precondicion si no esta en ninguno de los otros dos lobos, tiene q estar en este 

cazadoresSuperioresDeLobo nom   (Explorador n _ l1 l2)  = if nom == n then [] else if existeLoboEn nom l1
                                                                                        then cazadoresSuperioresDeLobo nom l1
                                                                                        else cazadoresSuperioresDeLobo nom l2 -- x precondicion si no esta en  el otros lobo, tiene q estar en este 




existeLoboEn :: Nombre -> Lobo -> Bool
existeLoboEn nom         (Cría n)        = nom == n 
existeLoboEn nom (Explorador n ts l1 l2) = nom == n || existeLoboEn nom l1 || existeLoboEn nom l2
existeLoboEn nom (Cazador n _ l1 l2 l3)  = nom == n || existeLoboEn nom l1 || existeLoboEn nom l2 || existeLoboEn nom l3



--Suponiendo la siguiente manada de ejemplo:
manadaEj = M (Cazador "DienteFiloso" ["Búfalos", "Antílopes"] (Cría "Hopito") 
                                                           (Explorador "Incansable" ["Oeste hasta el río"] (Cría "MechónGris") 
                                                                                                           (Cría "Rabito") ) 
                                                           (Cazador "Garras" ["Antílopes", "Ciervos"] (Explorador "Zarpado" ["Bosque este"] (Cría "Osado") 
                                                                                                                                            (Cazador "Mandíbulas" ["Cerdos", "Pavos"] (Cría "Desgreñado") 
                                                                                                                                                                                      (Cría "MalCríado") 
                                                                                                                                                                                      (Cazador "TrituraHuesos" ["Conejos"]  (Cría "Peludo") 
                                                                                                                                                                                                                            (Cría "Largo") 
                                                                                                                                                                                                                            (Cría "Menudo") ) ) ) 
                                                                                                      (Cría "Garrita") 
                                                                                                      (Cría "Manchas") ))

{-
--la función cazadoresSuperioresDe debería dar lo siguiente:
--cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
--cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
--cazadoresSuperioresDe "DienteFiloso" manadaEj = []
--cazadoresSuperioresDe "Peludo" manadaEj = ["DienteFiloso", "Garras", "Mandíbulas", "TrituraHuesos"]


{-
cazadoresSuperioresDe "I" manada3 = ["H", "A"]
cazadoresSuperioresDe "H" manada3 = ["A"]
cazadoresSuperioresDe "E" manada3 = ["A"]
cazadoresSuperioresDe "L" manada3 = ["K","H", "A"]
-}
-}