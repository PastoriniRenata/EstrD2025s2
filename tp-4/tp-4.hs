-- 1. Pizzas
-- Tenemos los siguientes tipos de datos:
data Pizza = Prepizza
        | Capa Ingrediente Pizza

data Ingrediente = Salsa
                   | Queso
                   | Jamon
                   | Aceitunas Int
--Denir las siguientes funciones:


--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--2. Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
                | Bifurcacion Cofre Mapa Mapa


--Denir las siguientes operaciones:
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica si al nal del camino hay un tesoro. Nota: el nal de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]





--Nave Espacial
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)
--Implementar las siguientes funciones utilizando recursión estructural:
--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]



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

data Manada = M Lobo
--Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--que corresponda en cada caso:

manada = M Cazador "Gonza" ["Pez", "Cierbo", "Conejo"] (Explorador "Azu" ["Rio", "Bosque"] (Cria "Rena") (Cria "Pablo"))
                                                       (Explorador "Ami" ["Lago"]          (Cria "Mar") (Cria "Fidel"))
                                                       (Cria "Mile" )


--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la can-
--tidad de crías.
buenaCaza :: Manada -> Bool

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.
elAlfa :: Manada -> (Nombre, Int)



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un terri-
--torio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los ca-
--zadores que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
--subordinado de un subordinado).
cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--Precondición: hay un lobo con dicho nombre y es único.



--Suponiendo la siguiente manada de ejemplo:
manadaEj = Cazador "DienteFiloso" ["Búfalos", "Antílopes"] (Cría "Hopito") 
                                                           (Explorador "Incansable" ["Oeste hasta el río"] (Cría "MechónGris") 
                                                                                                           (Cría "Rabito") ) 
                                                           (Cazador "Garras" ["Antílopes", "Ciervos"] (Explorador "Zarpado" ["Bosque este"] (Cría "Osado") 
                                                                                                                                            (Cazador "Mandíbulas" ["Cerdos", "Pavos"] (Cría "Desgreñado") 
                                                                                                                                                                                      (Cría "Malcriado") 
                                                                                                                                                                                      (Cazador "TrituraHuesos" ["Conejos"]  (Cría "Peludo") 
                                                                                                                                                                                                                            (Cría "Largo") 
                                                                                                                                                                                                                            (Cría "Menudo") ) ) ) 
                                                                                                      (Cría "Garrita") 
                                                                                                      (Cría "Manchas") )

--la función cazadoresSuperioresDe debería dar lo siguiente:
--cazadoresSuperioresDe "Mandíbulas" manadaEj = ["DienteFiloso", "Garras"]
--cazadoresSuperioresDe "Rabito" manadaEj = ["DienteFiloso"]
--cazadoresSuperioresDe "DienteFiloso" manadaEj = []
--cazadoresSuperioresDe "Peludo" manadaEj = ["DienteFiloso", "Garras", "Mandíbulas", "TrituraHuesos"]