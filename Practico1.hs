module Practico1 where

import Data.List
import Data.Maybe
import Language.Haskell.TH (FieldExp)

data Exp = Var X
            | Cons K [Exp]
            | Lam X Exp 
            | App Exp Exp
            | Case Exp [Br]
            | Rec X Exp
            | Iff Exp Exp Exp
            deriving (Show, Eq)

type X = String

type K = String

type Br = (K,([X],Exp))

data W = LamW X Exp
        | VarW X
        | ConsW K [Exp]
        | RecW X Exp
        deriving (Show, Eq)

type Sigma = [(X,Exp)]

bajas :: Sigma -> [String] -> Sigma
bajas sigma [] = sigma 
bajas sigma (v:vs) = bajas (filter (\(var, _) -> var /= v) sigma) vs

efectoBr :: Sigma -> [Br] -> [Br]
efectoBr s [] = []
efectoBr s ((k, (x, e)):bs) = (k, (x, efecto (bajas s x) e)) : efectoBr s bs


efecto :: Sigma -> Exp -> Exp
efecto s (Var x) = case lookup x s of
                Just e -> e
                Nothing -> Var x
efecto s (App e1 e2) = App (efecto s e1) (efecto s e2)
efecto s (Lam x e) = Lam x (efecto (bajas s [x]) e)
efecto s (Rec x e) = Rec x (efecto (bajas s [x]) e)
efecto s (Cons c e) = Cons c (map (efecto s) e)
efecto s (Case e br) = Case (efecto s e) (efectoBr s br)

evalW :: Exp -> W
evalW (Var x) = VarW x
evalW (Cons x eTecho) = ConsW x eTecho
evalW (Lam x e) = LamW x e
evalW (App e1 e2) = case evalW e1 of
        LamW x e -> evalW (efecto [(x, e2)] e)
        _ -> error "no froma adecuada"
evalW (Rec x e) = RecW x e
evalW (Case e bs) =
  case evalW e of
    ConsW k vs ->
      case lookup k bs of
        Just (xs, body) -> evalW (efecto (zip xs vs) body)
        Nothing -> error ("No matching branch for constructor: " ++ k)
    _ -> error "Case on non-constructor value"
evalW (Iff e e1 e2) = 
        case evalW e of
                ConsW True [] -> evalW e1
                ConsW False [] -> evalW e2
                _ -> error "Iff on non-boolean value"



-- Evaluador completo
evalC :: Exp -> Exp
evalC (Var x) = Var x
evalC (Lam x e) = Lam x (evalC e)
evalC (Rec x e) =
  evalC (efecto x (Rec x e) e)  -- regla Rec
evalC (App e1 e2) =
  case evalC e1 of
    Lam x body -> evalC (efecto x e2 body)  -- regla AppLam
    e1' -> App e1' (evalC e2)             -- recursiva en e2
evalC (Cons k es) = Cons k (map evalC es)
evalC (Case e brs) =
  case evalC e of
    Cons k es ->
      case lookup k brs of
        Just (xs, body) -> evalC (efecto (zip xs es) body) -- regla Case
        Nothing -> Case (Cons k es) brs                -- no match
    e' -> Case e' brs




orChi :: Exp
orChi = Lam "e1" (Lam "e2" (Case (Var "e1") [
        ("True", ([], Var "True")), 
        ("False", ([], Var "e2"))
        ]))

tripleChi :: Exp
tripleChi = Rec "triple" (Lam "n" (Case (Var "n") [
    ("0", ([], Cons "0" [])), 
    ("S", (["x"], Cons "S" [Cons "S" [App (Var "triple") (Var "x")]]))]))

duplicarChi :: Exp
duplicarChi = Rec "duplicar" (Lam "l" (Case (Var "l") [
        ("[]", ([], Cons "[]" [])), 
        (":", (["x", "xs"], Cons ":" [Var "x", Cons ":" [Var "x", App (Var "duplicar") (Var "xs")]]))
]))

ramaChi :: Exp
ramaChi = Rec "rama" (Lam "b" (Case (Var "b") [
        ("H", (["x"], Cons ":" [Var "x", Cons "[]" []])),
        ("N", (["i", "c", "d", "x"], Cons ":" [Var "x", App (Var "rama") (Var "c")]))
]))

takes :: Exp
takes = Rec "takes" (Lam "n" (Lam "l" (Case (Var "n") [
        ("0", ([], Cons "[]" [])),
        ("S", (["x","xs"], Cons ":" [Var "x", App (Var "takes") (Var "xs")]))
])))

notChi :: Exp
notChi = Lam "b" (Case (Var "b") [
        ("True", ([], Cons "False" [])),
        ("False", ([], Cons "True" []))
    ])
