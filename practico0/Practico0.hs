module Practico0 where

import Data.List
import Data.Maybe

data Exp = Var X | Emp | Uni Z | Bel Z Exp | Join Exp Exp | Inter Exp Exp | Diff Exp Exp | Incl Exp Exp | Asig X Exp | Pow Exp | Equ Exp Exp

data Val = IntList [Int] | BoolVal Bool | ListIntList [[Int]] deriving Show 

type Mem = [(X,Val)]

type X = String

type Z = Int

conj1 :: Exp
conj1 = Join (Join (Uni 1) (Uni 2)) (Uni 3)

conj2 :: Exp
conj2 = Join (Join (Uni 2) (Uni 3)) (Uni 4)

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
ass1 = Asig ("w") conj1

ass2 :: Exp
ass2 = Asig ("x") conj4

ass3 :: Exp
ass3 = Asig ("y") pert2

ass4 :: Exp
ass4 = Asig ("z") incl2

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

belongs :: Int -> [Int] -> Bool
belongs n [] = False
belongs n (x:xs)
    | n == x = True
    | otherwise = belongs n (xs)

unioni :: [Int] -> [Int] -> [Int]
unioni [] n = n
unioni (x:xs) n = (x:(unioni xs n))

intersection :: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) (y:ys) 
    | x == y = (x:(intersection xs ys))
    | otherwise = ((intersection (x:xs) ys) ++ (intersection xs (y:ys)))

difference :: [Int] -> [Int] -> [Int]
difference [] n = []
difference n [] = n
difference (x:xs) (y:ys)
    | x == y = (difference xs ys)
    | otherwise = x:(difference xs ys)

included :: [Int] -> [Int] -> Bool
included [] _ = True
included x [] = False
included (x:xs) (y:ys) 
    | x == y = included xs (y:ys)
    | otherwise = included (x:xs) ys && included xs (y:ys)

power :: [Int] -> [[Int]]
power [] = [[]]
power (x:xs) = 
    let ps = power xs 
    in (map (x:) ps) ++ ps

igual :: Ord Int => [Int] -> [Int] -> Bool
igual x y = nub (sort x) == nub (sort y)

eval :: Mem -> Exp -> (Mem, Val)
eval m (Var x) = 
    let Just b = lkup x m
    in (m, b)
eval m (Asig x e) =
    let (m', v) = eval m e
    in (upd x v m', v)
eval m (Emp) = (m, IntList [])
eval m (Uni x) = (m, IntList [x])
eval m (Bel z e) =
    let IntList x = snd(eval m e)
        v = belongs z x
    in (m, BoolVal v)
eval m (Join e1 e2) = 
    let IntList x = snd(eval m e1)
        IntList y = snd(eval m e2)
        v = nub(unioni x y)
    in (m, IntList v)
eval m (Inter e1 e2) =
    let IntList x = snd(eval m e1)
        IntList y = snd(eval m e2)
        v = nub(intersection x y)
    in (m, IntList v)
eval m (Diff e1 e2) =
    let IntList x = snd(eval m e1)
        IntList y = snd(eval m e2)
        v = nub(difference x y)
    in (m, IntList v)
eval m (Incl e1 e2) =
    let IntList x = snd(eval m e1)
        IntList y = snd(eval m e2)
        v = included x y
    in (m, BoolVal v)
eval m (Pow e) = 
    let IntList x = snd(eval m e)
        v = power x
    in (m, ListIntList v)
eval m (Equ e1 e2) =
    let IntList x = snd (eval m e1) 
        IntList y = snd (eval m e2)
        v = igual x y
    in (m, BoolVal v)


