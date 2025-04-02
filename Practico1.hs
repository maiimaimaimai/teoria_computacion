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
            deriving (Show, Eq)

type X = String

type K = String

type Br = (K,([X],Exp))

data W = LamW X Exp
        | CoseW K [Exp]
        deriving (Show, Eq)

type Sigma = [(X,Exp)]

bajas :: Sigma -> [X] -> Sigma
bajas = undefined

efecto :: Sigma -> Exp -> Exp
efecto = undefined
efecto s (App e1 e2) = App (efecto s e1) (efecto s e2)
efecto s (Rec x e) = Rec x (efecto (bajas s [x]) e)

evalW :: Exp -> W
evalW (Cons x eTecho) = ConsW x eTecho
evalW ...

