module Practico2 where

import Data.List
import Data.Maybe

data Prog = Seq Prog Prog
            | Asig [X] [Exp]
            | Case X [Br]
            | While X [Br]
            | Local [X] Prog
        
type K = String

data Br = ((K,[X]),Prog)

data Exp = Cons K [Exp]
            | Var X

data Val = ConsVal K [Val]
            | Null

type Mem = [(X,Val)]

lkup :: Eq X => X -> Mem -> Maybe Val 
lkup k [] = Nothing
lkup k ((k1,v1):kvs)
    | k == k1 = Just v1
    | otherwise = lkup k kvs

upd :: Eq X => X -> Val -> Mem -> Mem
upd k v [] = [(k,v)]
upd k v ((k1,v1):kvs)
    | k == k1 = ((k1,v):kvs)
    | otherwise = ((k1,v1):(upd k v kvs))

altas :: [X] -> Mem -> Mem
altas [] m = m
altas (x:xs) m = (x,Null):(alta xs m)

baja :: X -> Mem -> Mem
baja x [] = []
baja x ((a,b):abs)
        | a == x = abs 
        | otherwise = (a,b:bajas2 x abs)

bajas :: [X] -> Mem -> Mem
bajas [] m = m
bajas xs [] = []
bajas (x:xs) m = baja xs (bajas2 x m)

-- bajas :: Eq X => [X] -> Mem -> Mem
-- bajas [] m = m
-- bajas (x:xs) ((k1,v1):kvs) = 
--     | k1 == x = bajas xs kvs
--     | otherwise = (k1,v1):(bajas xs kvs)

evalExp :: Mem -> Exp -> Val
evalExp m Var x = 
    case lkup x m of
        Nothing -> error "not in memory"
        Just v -> v
evalExp m (Cons k []) = K []
evalExp m [Cons k (e:es)] = ConsVal k (lkup e m:evalExp m es)   