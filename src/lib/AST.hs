module AST where

data Timescale = Timescale Integer Integer
    deriving Show

data LV = Zero | One | Unknown | DontCare | Dash | Every
    deriving Show
data Trans = Basic LV | Tuple LV LV
    deriving Show

data Primitive = Primitive {
    ports :: [String]
    , inputs :: [String]
    , outputs :: [String]
    , regs :: [String]
    , table :: [[Trans]]
}
    deriving Show

data AST = AST {
    timescale :: Timescale
    , primitives :: [Primitive]
}
    deriving Show
