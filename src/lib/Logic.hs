module Logic where

data Net t = Net t
    deriving Show

data Port t = 
    And (Net t) [(Net t)]
    | Or (Net t) [(Net t)]
    | Not [(Net t)] (Net t)
    | Buf [(Net t)] (Net t)
    deriving Show

data Reg t = Reg t
    deriving Show

data SType = 
    PosEdge 
    | NegEdge 
    | AnyEdge
    deriving Show

data Sens t = Sens (Net t) SType
    deriving Show

mkSens :: [t] -> [Sens t]
mkSens = map (\x -> Sens (Net x) AnyEdge)

nets :: [t] -> [Net t]
nets = map Net

data Proc t = Proc [Sens t] (Port t)
    deriving Show

data ProcState t = ProcState [Reg t]
    deriving Show

data Module t = Module { 
    inputs :: [t]
    , outputs :: [t]
    , procs :: [Proc t]
} deriving Show

testCircuit = And (Net "o") [Net "a", Net "b"]

testModule = Module {
    inputs = ["d", "clk", "clr", "set", "notifier"]
    , outputs = ["q"]
    , procs = [
        Proc 
            (mkSens ["d", "clk", "clr", "set", "notifier"])
            (And (Net "q") [])
    ]
}