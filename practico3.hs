module Practico3 where

import Data.List
import Data.Maybe
import Language.Haskell.TH (FieldExp)
import Text.Read (Lexeme(Symbol))

type Tape = ([Symbol],Symbol, [Symbol])

data Symbol = Blank | Cons K | Joker 
    deriving (Show, Eq)

type K = String

type State = String

-- data Nat = Zero | Suc Nat
--     deriving (Show, Eq)

data Action = MoveLeft | MoveRight | Write Symbol
    deriving (Show, Eq)

type Code = [(State, [Branch])]

type Branch = (Symbol, (Action, State))

lookupSymbol :: Symbol -> [Branch] -> Maybe (Action, State)
lookupSymbol s [] = Nothing
lookupSymbol s ((s', b):xs)
    | s == s' = Just b
    | otherwise = case lookupSymbol s xs of
        Nothing -> if s' == Joker then Just b else Nothing
        justValue -> justValue

lookupInCode :: State -> Code -> Maybe [Branch]
lookupInCode s [] = Nothing
lookupInCode s ((s', b):xs) =
    if s == s' then Just b
    else lookupInCode s xs

step :: Tape -> [Branch] -> (Tape, State)
step ([], s, r) br = step ([Blank], s, r) br
step (l, s, []) br = step (l, s, [Blank]) br
step (l, s , r) br = case lookupSymbol s br of
        Just (a, q) -> case a of
                MoveLeft -> ((init l, last l, s:r), q)
                MoveRight -> ((s:l, head r, tail r), q)
                Write s' -> ((l, s', r), q)
        Nothing -> error "Symbol not found"

iteration :: Tape -> State -> Code -> (Tape, State)
iteration t q c = case q of
        "h" -> (t, q)
        _ -> case lookupInCode q c of
                Nothing -> error "State not found"
                Just br -> let (t', q') = step t br in iteration t' q' c


excecute :: Code -> Tape -> Tape
excecute c t = fst(iteration t ("i") c)

leftSigma :: Tape -> Symbol -> Tape
leftSigma ([], s, r) sym =
    if sym == s then ([], s, r)
    else leftSigma ([], Blank, s:r) sym  -- extendemon con blank
leftSigma (l, s, r) sym 
    | sym == s = (l, s, r)  -- su encomtramos el simbolo paramos
    | otherwise =   
        let (l',s') = case l of
                [] -> ([], Blank)  -- si no hay nada a la izquierda, ponemos un blank
                _ -> (init l, last l)
        in leftSigma (l', s', s:r) sym  -- recursión

-- Par 
-- | Estado | Símbolo | Acción | Nuevo estado |
-- | ------ | ------- | ----- | ------------ |
-- | i      | σ       | R     | even         |
-- | even   | σ       | R     | odd          |
-- | odd    | σ       | R     | even         |
-- | even   | _       | _     | h            |
-- | odd    | _       | _     | h            |

par :: Code
par = [("i", [(Joker, (MoveRight, "even"))]),
       ("even", [(Joker, (MoveRight, "odd")), (Blank, (Write Blank, "h"))]),
       ("odd", [(Joker, (MoveRight, "even")), (Blank, (Write Blank, "h"))])]

-- Elemσ
-- i -(σ1, σ1, _)-> h
--   -(σ2, σ2, R)-> i
--   -(#, #, _)-> i

elemSigma :: String -> Code
elemSigma sym = [("i",
                    [(Joker, (MoveRight, "lookFor"))]),

                ("lookFor",
                    [(Cons sym, (MoveRight, "found")),
                     (Blank, (MoveRight, "notFound")),
                     (Joker, (MoveRight, "lookFor"))]),

                ("found",
                    [(Blank, (Write Blank, "h"))]),
                 
                ("notFound",
                    [(Blank, (Write Blank, "h"))])]

--Reverse


-- PARTE 2
-- 1
  -- a)
  -- b) Ambas tienen a misma cantidad de casillas (infinita de ambos lados = infinita de la derecha)
        -- MT ... |4|2|0|1|3|5| ...
        -- MTr [0|1|2|3|4|5|...
        -- L' : LL
        -- R' : RR
        -- W's : Ws
            -- puedo meter una cinta en la otra y las acciones son las mismas 

-- 2
-- a) bˉ:(q,σ1​,σ2​,…,σk​) -> ((γ1​,γ2​,…,γk​),(d1​,d2​,…,dk​),q′)
    -- Cada γi​: símbolo a escribir en la celda del cabezal i
    -- Cada di∈{L,R,S}: dirección de movimiento (Left, Right o Stay) del cabezal i
    -- q′: nuevo estado.
-- b) Las MT son lo más potente que hay, pueden simular cualquier otra máquina de Turing

-- 3
-- a) add moveDown moveUp
