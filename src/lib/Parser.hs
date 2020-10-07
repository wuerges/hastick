{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Parser where 


import Text.Parsec.String  (Parser, parseFromFile)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Token  as P
import Text.Parsec.Language (javaStyle)

import Control.Arrow

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
    

tableRow = do
    part1 <- many logicValue
    part2 <- many (colon >> logicValue)
    _ <- semi
    return $ part1 ++ part2


parseTable :: Parser [[Trans]]
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
    regs <- option [] (decl "reg")
    tabl <- parseTable
    reserved "endprimitive"
    return $ Primitive ios inputs outputs regs tabl


parseVerilog :: Parser AST
parseVerilog = do
    ts <- parseTimescale
    ps <- many primitive
    eof
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
lexeme = P.lexeme lexer


tupleValue :: Parser (LV, LV)
tupleValue = 
    parens $ do 
        a <- basicValue
        b <- basicValue
        return (a, b)

basicValue :: Parser LV
basicValue = 
        (char '0' >> return Zero)
    <|> (char '1' >> return One)
    <|> (char '?' >> return Unknown)
    <|> (char '*' >> return Every)
    <|> (char 'x' >> return DontCare)
    <|> (char '-' >> return Dash)


logicValue :: Parser Trans
logicValue = lexeme
   (    (basicValue >>= return . Basic)
    <|> (tupleValue >>= return . uncurry Tuple)
   )
