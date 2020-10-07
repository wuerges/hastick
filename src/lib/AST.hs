module AST where

data Timescale = Timescale Integer Integer
    deriving Show

data Primitive = Primitive {
    ports :: [String]
    , inputs :: [String]
    , outputs :: [String]
    , table :: [[String]]
}
    deriving Show

data AST = AST {
    timescale :: Timescale
    , primitives :: [Primitive]
}
    deriving Show
