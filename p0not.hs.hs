module p0not where

import Data.List
import Data.Maybe

data Exp = Var X | Emp | Uni Z | Bel Z Exp | Join Exp Exp | Inter Exp Exp | Diff Exp Exp | Incl Exp Exp | Asig X Exp | Pow Exp | Equal Exp Exp
data Val = IntList [Int] | BoolVal Bool
    deriving (Show)
-- data E = []

type Mem = [(X, Val)]
type X = String
type Z = Int

conj1 :: Exp
conj1 =  Join (Join (Uni 1) (Uni 2)) (Uni 3)

conj2 :: Exp
conj2 =  Join (Join (Uni 2) (Uni 3)) (Uni 4)

conj3 :: Exp
conj3 = Join conj1 conj2

conj4 :: Exp
conj4 = Inter conj1 conj2

pert1 :: Exp
pert1 = Bel 2 conj1

pert2 :: Exp
pert2 = Bel 3 conj4

incl1 :: Exp
incl1 = Incl conj1 conj2

incl2 :: Exp
incl2 = Incl conj4 conj2

incl3 :: Exp
incl3 = Incl conj1 conj3

ass1 :: Exp
ass1 = Asig "w" conj1

ass2 :: Exp
ass2 = Asig "x" conj4

ass3 :: Exp
ass3 = Asig "y" pert2

ass4 :: Exp
ass4 = Asig "z" incl2

eval :: Exp -> Mem -> Val
eval Emp _ = IntList []
eval (Uni z) _ = IntList [z]
eval (Join e1 e2) m = 
    let IntList s1 = eval e1 m
        IntList s2 = eval e2 m
    in IntList (nub (s1 ++ s2)) --nub elimina duplicados 
eval (Inter e1 e2) m = 
    let IntList s1 = eval e1 m
        IntList s2 = eval e2 m
    in IntList (filter (`elem` s2) s1) --filter filtra los elementos de s1 que están en s2
eval (Diff e1 e2) m = 
    let IntList s1 = eval e1 m
        IntList s2 = eval e2 m
    in IntList (filter (`notElem` s2) s1) --filter (notelem) filtra los elementos de s1 que no están en s2
eval (Bel z e) m = 
    let IntList s = eval e m
    in BoolVal (z `elem` s) --devuelve bool si z está en s
eval (Incl e1 e2) m = 
    let IntList s1 = eval e1 m
        IntList s2 = eval e2 m
    in BoolVal (all (`elem` s2) s1) --all para verificar si todos los elementos de s1 están en s2
eval (Asig x e) m = eval e m
eval (Var x) m = fromMaybe (IntList []) (lkup x m) --busca una variable x en la memoria m
                                                        --si x está en la memoria, devuelve su valor
                                                        --si x no existe, devuelve el conjunto vacío []
eval (Pow e) m = let IntList s = eval e m
                  in IntList (map length (subsequences s))
eval (Equal e1 e2) m = 
    let IntList s1 = eval e1 m
        IntList s2 = eval e2 m
    in BoolVal (nub s1 == nub s2)

lkup :: X -> Mem -> Maybe Val
lkup k [] = Nothing
lkup k ((k1,v1):kvs)
    | k == k1 = Just v1
    | otherwise = lkup k kvs

upd :: X -> Val -> Mem -> Mem
upd k v [] = [(k,v)]
upd k v ((k1,v1):kvs)
    | k == k1 = ((k1,v):kvs)
    | otherwise = ((k1,v1):(upd k v kvs))
