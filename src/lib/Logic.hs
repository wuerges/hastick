module Logic where

data Wire t = Wire t
    deriving Show
data Port t = 
    And (Wire t) [(Wire t)]
    | Or (Wire t) [(Wire t)]
    | Not [(Wire t)] (Wire t)
    | Buf [(Wire t)] (Wire t)
    deriving Show


testCircuit = And (Wire "o") [Wire "a", Wire "b"]
