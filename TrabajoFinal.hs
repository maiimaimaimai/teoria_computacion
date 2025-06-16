
data Exp = Var String | Neg String 

type Clause = (Exp, Exp, Exp)
type DomA = [Clause]
data SolA = SAT | INSAT


type Node = (Name, Cost, Benefit)
--Node = Project
type Edge = (Node, Node)
type Graph = ([Node], [Edge])

type Cost = Int
type Benefit = Int
type Name = String
type Path = [Edge] 

type MV = (Cost, Benefit)
type DomB = Graph
type SolB = ((Path, MV), (Cost, Benefit))

verifyA :: (DomA, SolA) -> Bool
verifyA (domA, solA) = undefined

verifyB :: (DomB, SolB) -> Bool
verifyB (domB, solB) = (length (fst (fst solB)) > 0) &&
                    (fst (snd (fst solB))) >= (fst (snd solB)) &&
                        (snd (snd (fst solB))) <= (snd (snd solB))
