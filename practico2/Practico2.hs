module Practico2 where

import Data.List
import Data.Maybe
import Practico0 (igual)

data Prog = Seq Prog Prog
            | Asig [X] [Exp]
            | Case X [Br]
            | While X [Br]
            | Local [X] Prog
            | Env K [X] Prog
            | Fun K [Exp]
        
type K = String

type X = String

type Br = ((K,[X]),Prog)

data Exp = Cons K [Exp]
            | Var X

data Val = ConsVal K [Val]
            | Null
    deriving (Show)     

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
altas (x:xs) m = (x,Null):(altas xs m)

baja :: X -> Mem -> Mem
baja x [] = []
baja x ((a,b):abs)
        | a == x = abs 
        | otherwise = (a,b):(baja x abs)

bajas :: [X] -> Mem -> Mem
bajas [] m = m
bajas xs [] = []
bajas (x:xs) m = bajas xs (baja x m)

-- bajas :: Eq X => [X] -> Mem -> Mem
-- bajas [] m = m
-- bajas (x:xs) ((k1,v1):kvs) = 
--     | k1 == x = bajas xs kvs
--     | otherwise = (k1,v1):(bajas xs kvs)

evalExp :: Mem -> Exp -> Val
evalExp m (Var x) = 
    case lkup x m of
        Nothing -> error "not in memory"
        Just v -> v
evalExp m (Cons k es) = 
    case es of 
        [] -> ConsVal k []
        (e:es) -> ConsVal k (map (evalExp m) es)  

asigListas :: [X] -> [Val] -> Mem -> Mem
asigListas [] [] m = m 
asigListas (x:xs) (v:vs) m = (upd x v (asigListas xs vs m))

lookInBranch :: Eq K => K -> [Br] -> Maybe ([X],Prog)
lookInBranch k [] = Nothing
lookInBranch k (b:br)
                | k == fst(fst b) = Just (snd(fst b),snd b)
                | otherwise = lookInBranch k br

evalProg :: Mem -> Prog -> Mem
evalProg m (Seq p1 p2) = evalProg (evalProg m p1) p2
evalProg m (Asig xs es) = 
                        let vs = (map (evalExp m) es) 
                        in asigListas xs vs m
evalProg m (Case x br) = case evalExp m (Var x) of
                                ConsVal k vs -> case lookInBranch k br of
                                                        Just (xs, p) -> 
                                                            if length xs == length vs 
                                                                then (evalProg m (Local xs p))
                                                            else error "Los largos no coinciden"
                                                        Nothing -> error "No se encontro el caso del constuructor"   
                                _ -> error "No devolvio un contructor"                                 
evalProg m (While x br) = case evalExp m (Var x) of
                                ConsVal k vs -> case lookInBranch k br of
                                                        Just (xs, p) -> 
                                                            if length xs == length vs 
                                                                then (evalProg (evalProg m (Local xs p)) (While x br))
                                                            else error "Los largos no coinciden"
                                                        Nothing -> m  
                                _ -> error "No devolvio un contructor"  
evalProg m (Local xs p) = bajas xs (evalProg (altas xs m) p)
-- evalProg d m (Fun k es) =
--     case lookup f d of
--         Just (xs, p) ->     -- si encontramos la funcion en el entorno
--             let vs = map (evalExp m) es
--             in if length xs == length vs
--                 then
--                     let m' = asigListas xs vs m
--                         in evalProg m' p

notImp :: X -> Prog
notImp x = Case "b"
        [(("True",[]), Asig ["result"] [Cons "True" []]),
         (("False",[]), Asig ["result"] [Cons "False" []])]

parImp :: Prog
parImp = Local ["n'"]
    (Seq    
        (Asig ["n'", "result"] [Var "n", Cons "True" []])
        (While "n'" 
                [(("Sucessor", ["x"]), 
                    Seq 
                        (Case "result"
                            [(("True",[]), Asig ["result"] [Cons "True" []]),
                            (("False",[]), Asig ["result"] [Cons "False" []])])  
                        (Asig ["n'"] [Var "x"]))]
        )
    )

memory = [("n", ConsVal "Sucessor" [ConsVal "Sucessor" []], ("result", Null))]
        
sumaImp :: Prog
sumaImp = Local ["n'", "res" ]
    (Seq 
        (Asig ["n'", "res"] [Var "n", Var "m"])
        (Seq
            (While "n'"
                [(("Sucessor", ["x"]),
                    Seq 
                        (Asig ["res"] [Cons "Sucessor" [Var "res"]])
                        (Asig ["n'"] [Var "x"])
                )]
            )
            (Asig ["result"] [Var "res"]) 
        )
    )

lengthImp :: Prog
lengthImp = Local ["l'"]
    (Seq 
        (Asig ["l'", "result"] [ Var "l", Cons "0" []])
        (Seq
            (While "l'"
                [((":", ["x", "xs"]),
                    Seq
                        (Asig ["result"] [Cons "Sucessor" [Var "result"]])
                        (Asig ["l'"] [Var "xs"])
                    )]
                )
                (Asig ["result"] [Var "result"])
            )
        )

-- igualdadImp :: Prog
-- igualdadImp = Local ["x", "y"]
--     (Seq 
--         (Asig ["x", "y", "result"] [Var "x", Var "y", Cons "True" []])
--         (Seq
--             (While "x"
--                 [(("x", ["Sucessor", "xx"]),
--                     Case "y'"
--                         [(("Succesor", "xy"),
--                                 (Asig ["x'", "y'"] [Var "xx", Var "xy"])
--                         ),
--                         (("0", []),
--                             Seq 
--                                 (Asig ["result"] [Cons "False" []])
--                                 (Asig ["x'"] [Cons "0" []])
--                         )
--                         ]
--                 )]
--             )
--         )
--     )

concatImp :: Prog
concatImp = Local ["l1", "l2", "result"]
    (Seq 
        (Asig ["l1", "l2", "result"] [Var "l1", Var "l2", Cons "Nil" []])
        (Seq
            (While "l1"
                [((":", ["x", "xs"]),
                    Seq
                        (Asig ["reuslt"] [Cons ":" [Var "x", Var "result"]])
                        (Asig ["l1"] [Var "xs"])
                    )]
                )
            (While "l2"
                [((":", ["x", "xs"]),
                    Seq
                        (Asig ["result"] [Cons ":" [Var "x", Var "result"]])
                        (Asig ["l2"] [Var "xs"])
                    )]
                )
            )
        (Asig ["result"] [Var "result"])
    )

