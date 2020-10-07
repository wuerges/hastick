{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Parser where 


import Text.Parsec.String  (Parser, parseFromFile)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Token  as P
import Text.Parsec.Language (javaStyle)

import AST


parseTimescale :: Parser Timescale
parseTimescale = do
    _ <- reserved "`timescale"
    x <- natural
    _ <- identifier
    _ <- char '/'
    y <- natural
    _ <- identifier
    return $ Timescale x y

decl keyword = do
    _ <- reserved keyword
    outputs <- commaSep identifier
    _ <- semi
    return outputs
    

col :: Parser String
col = identifier
    

tableRow = do
    part1 <- many col
    part2 <- many (colon >> col)
    _ <- semi
    return $ part1 ++ part2


parseTable :: Parser [[String]]
parseTable = do
    _ <- reserved "table"
    rows <- many tableRow
    _ <- reserved "endtable"
    return rows
    

primitive :: Parser Primitive
primitive = do
    _ <- reserved "primitive"
    name <- identifier
    ios <- parens (commaSep identifier)
    _ <- semi
    outputs <- decl "output"
    inputs <- decl "input"
    tabl <- parseTable
    return $ Primitive ios inputs outputs tabl


parseVerilog :: Parser AST
parseVerilog = do
    ts <- parseTimescale
    ps <- many primitive
    return $ AST { timescale = ts, primitives = ps }

parseFile :: FilePath -> IO (Either ParseError AST)
parseFile path = parseFromFile parseVerilog path



-- the Lexer
lexer = P.makeTokenParser javaStyle
parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
natural = P.natural lexer
commaSep = P.commaSep lexer
semi = P.semi lexer
colon = P.colon lexer
stringLiteral = P.stringLiteral lexer