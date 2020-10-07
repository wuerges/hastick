module Parser where 

import Text.Parsec.Prim
import Text.Parsec.Combinator

import AST


parseFile :: IO AST
parseFile = undefined