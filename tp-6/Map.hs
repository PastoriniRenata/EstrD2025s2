{-
La interfaz del tipo abstracto Map es la siguiente:

1. Como una lista de pares-clave valor sin claves repetidas
-}

module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [(k,v)]
    deriving Show
    
    



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--VERSION 1 DE MAP (NO ACEPTA CLAVES REPETIDAS)

--Propósito: devuelve un map vacío
emptyM :: Map k v -- --> O(1)
emptyM = M []


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: agrega una asociación clave-valor al map.
-- OBS mia: Si la clave dada ya se encuentra en el map, pisa el valor anterior por el dado. Agrega al final del map
assocM :: Eq k => k -> v -> Map k v -> Map k v -- tiene costo O(n), donde n es la cantidad de elementos en el map. 
assocM k v (M ls) =  M (asociar k v ls)

asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)] 
--OBS: en caso de que ya exista una tupla con esa clave, la pisa con la nueva. En caso de que no exista la agrega 
-- al final
asociar c val    []    = [(c,val)]
asociar c val (cv:cvs) = if coincideClave c cv
                                    then (c,val):cvs
                                    then cv:(asociar c val cvs)

coincideClave :: Eq k => k -> (k,v) -> Bool
coincideClave c1 (c2,_) = c1 == c2

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM c (M ls) = buscar c ls

buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar _    []    = Nothing
buscar c (cv:cvs) = if coincideClave c cv
                                then Just y
                                else buscar c cvs


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM c (M ls) = M (borrarDeLs c)


borrarDeLs :: Eq k => k -> [(k,v)] -> [(k,v)]
--OBS mia: como no acepta claves repetidas, con encontrarla una vez me alcanza, no existen repeticiones que borrar
borrarDeLs _    []    = error"no existe un valor asociado a esa clave"
borrarDeLs c (cv:cvs) = if coincideClave c cv 
                                then cvs
                                else cv : borrarDeLs c cvs

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M cvs) = claves cvss

claves :: []




