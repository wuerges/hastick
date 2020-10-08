{-# LANGUAGE DuplicateRecordFields #-}
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

data VProcess = VProcess {
    name :: String
}
    deriving Show

data VModule = VModule {
    name :: String
    , ports :: [String]
    , inputs :: [String]
    , outputs :: [String]
    , regs :: [String]
    , procs :: [VProcess]
}
    deriving Show


data Decl = Regs [String] 
          | Wire [String] 
          | Always 
          | Inputs [String] 
          | Outputs [String]

makeVModule :: String -> [Decl] -> VModule
makeVModule name decls = vmod
    where vmod = 
            VModule { 
                name = name
                , ports = []
                , inputs = []
                , outputs = []
                , regs = []
                , procs = []
            }

data Entity = VModuleEntity VModule | PrimitiveEntity Primitive
    deriving Show
    
data AST = AST {
    timescale :: Timescale
    , entities :: [Entity]
}
    deriving Show
