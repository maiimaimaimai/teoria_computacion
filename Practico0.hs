module Practico0 where

import Data.List
import Data.Maybe

data Exp = Var X | Emp | Uni Z | Bel Z Exp | Join Exp Exp | Inter Exp Exp | Diff Exp Exp | Incl Exp Exp | Asig X Exp
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
