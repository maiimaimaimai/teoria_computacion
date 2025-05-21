module Practico1 where

import Data.List
import Data.Maybe
import Language.Haskell.TH (FieldExp)

type Tape = ([Symbol],Symbol, [Symbol])

data Symbol = Blank | Cons K | Joker 

type K = String

type State = String

data Nat = Zero | Suc Nat

data Action = MoveLeft | MoveRight | Write Symbol

type Code = [(State, [Branch])]

type Branch = (Symbol, (Action, State))

lookupSymbol :: Eq Symbol => Symbol -> [Branch] -> Maybe (Action, State)
lookupSymbol s [] = Nothing
lookupSymbol s ((s', b):xs) =
        if s == s' then Just b
        else lookupSymbol s xs

lookupInCode :: State -> Code -> Maybe [Branch]
lookupInCode s [] = Nothing
lookupInCode s ((s', b):xs) =
        if s == s' then Just b
        else lookupInCode s xs

step :: Tape -> [Branch] -> (Tape, State)
step (l, s , r) br = case lookupSymbol s br of
        Just (a, q) -> case a of
                MoveLeft -> ((init l, last l, [s]++r), q)
                MoveRight -> (([s]++l, head r, tail r), q)
                Write s' -> ((l, s', r), q)
        Nothing -> error "Symbol not found"

-- goToInitial :: Tape -> Tape

iteration :: Tape -> State -> Code -> (Tape, State)
iteration t q c = case q of
        "h" -> (t, q)
        _ -> case lookupInCode q c of
                Nothing -> error "State not found"
                Just br -> let (t', q') = step t br in iteration t' q' c


excecute :: Code -> Tape -> Tape
excecute c t = fst(iteration t ("i") c)