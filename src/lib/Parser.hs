{-# LANGUAGE FlexibleContexts, OverloadedStrings, DuplicateRecordFields, RecordWildCards #-}
module Parser where 


import Text.Parsec.String  (Parser, parseFromFile)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

import Control.Arrow
import Control.Monad.State

import AST
import Logic


-- data ParserState = Empty | VMod VModule

-- type Parser = Parsec String ParserState

-- type Parser s t = ParsecT s () IO

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


parseVModule :: Parser VModule
parseVModule = do
    reserved "module"
    name <- identifier
    ports <- parens (commaSep identifier)
    semi
    decls <- betend (reserved "endmodule") parseDecl
    return $ makeVModule name decls

event :: Parser ()
event = ((reserved "posedge" >> identifier >> return ())
    <|> (reserved "negedge" >> identifier >> return ())
    <|> (identifier >> return ())
    <|> parens event) >> option () (reserved "or" >> event)

parseSensList = symbol "@" >> parens event


logicOperator = reserved "or"
            <|> reserved "and"
            <|> (symbol "+" >> return ())
            <|> (symbol "-" >> return ())

packedArray = braces (commaSep identifier)

literalValue = lexeme $ natural >> symbol "'" >> (oneOf "hbdo") >> alphaNum

valueExpr = ((symbol "~" >> valueExpr)
         <|> (packedArray  >> return ())
         <|> (literalValue  >> return ())
         <|> (identifier  >> return ())
         <|> parens valueExpr) >> option () (logicOperator >> valueExpr)

attr = valueExpr >> symbol "<=" >> valueExpr

expr = (reserved "if" >> parens valueExpr >> parseBlock >>
            (option () (reserved "else" >> parseBlock)))
   <|> (attr >> semi >> return ())


bet b e f = b >> betend e f
betend e f = (e >> return [])
         <|> (do x <- f; m <- betend e f; return (x:m))

block f = bet (reserved "begin") (reserved "end") f


parseBlock = (block expr >> return ())
            --  (reserved "begin" >> many expr >> reserved "end")
         <|> expr

parseAlways :: Parser Decl
parseAlways = do
    reserved "always"
    parseSensList
    parseBlock
    return Always



parseDecl :: Parser Decl
parseDecl = 
        (decl "reg" >>= return . Regs)
    <|> (decl "wire" >>= return . Wire)
    <|> (decl "input" >>= return . Inputs)
    <|> (decl "output" >>= return . Outputs)
    <|> parseAlways

-- parseVModuleBody :: StateT VModule Parser ()
-- parseVModuleBody = 
--     parseRegsDecl <|> parseRegsDecl

-- parseRegsDecl :: StateT VModule Parser ()
-- parseRegsDecl = 
--     (do decl_regs <- lift $ decl "regs"
--         modify $ \x@VModule{regs = rs, ..} -> (x :: VModule) { regs = decl_regs ++ rs}
--     )

    
parseVerilog :: Parser AST
parseVerilog = do
    ts <- parseTimescale
    ps <- many $ (primitive >>= return . PrimitiveEntity) 
    reserved "`celldefine"
    ms <- many $ (parseVModule >>= return . VModuleEntity)
    eof
    return $ AST { timescale = ts, entities = ps ++ ms }

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
symbol = P.symbol lexer 


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
