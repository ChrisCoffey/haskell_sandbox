module While where

import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token            as Token

type VariableName = String
data BBinOp = And | Or  
    deriving (Show)
data RBinOp = Gt | Lt   
    deriving (Show)

data BExpr = BConst Bool
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving(Show)

data ABinOp = Add 
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data AExpr = NConst Integer
           | Variable VariableName
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data Statement = Many [Statement]
               | Assign VariableName AExpr
               | If BExpr Statement Statement
               | While BExpr Statement
               deriving (Show)

langDef = 
    emptyDef { Token.commentStart    = "/*",
               Token.commentEnd      = "*/",
               Token.commentLine     = "//",
               Token.identStart      = letter,
               Token.identLetter     = alphaNum,
               Token.reservedNames   = ["If", 
                                        "then",
                                        "else",
                                        "while",
                                        "do",
                                        "true",
                                        "false",
                                        "and",
                                        "or"],
                Token.reservedOpNames = ["+", "-", "/", "*",
                                         ":=",
                                         "<", ">",
                                         "and", "or"]
    }

lexer = Token.makeTokenParser langDef

ident       = Token.identifier  lexer
reserved    = Token.reserved    lexer
reservedOp  = Token.reservedOp  lexer
parens      = Token.parens      lexer
braces      = Token.braces      lexer
integer     = Token.integer     lexer
semiC       = Token.semi        lexer
wSpace      = Token.whiteSpace  lexer

--Expression Parsers
--Arithmetic
aOps = 
    [ [Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
       Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft],
      [Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft,
       Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]]

aTerm = parens aExpression 
    <|> liftM Variable ident
    <|> liftM NConst integer

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOps aTerm

--Boolean
bOps = 
    [ [Infix (reservedOp "and" >> return (BBinary And)) AssocLeft,
       Infix (reservedOp "or"  >> return (BBinary Or )) AssocLeft]]

bTerm = parens bExpression
    <|> (reserved "true"  >> return (BConst True))
    <|> (reserved "false" >> return (BConst False))
    <|> rExpression

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOps bTerm

--Relational
relation = 
    (reservedOp ">" >> return Gt) <|> (reservedOp "<" >> return Lt) 

rExpression = do
    l <- aExpression
    op <- relation
    r <- aExpression
    return $ RBinary op l r

-- Statement Parsers
ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    yes <- statement
    reserved "else"
    no  <- statement 
    return $ If cond yes no

whileStatement = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    body <- braces statement
    return $ While cond body

assignStatement = do
    name <- ident
    reservedOp ":="
    expr <- aExpression
    return $ Assign name expr

statement :: Parser Statement
statement = parens statement <|> manyStatements

statement' :: Parser Statement
statement' = ifStatement
         <|> whileStatement
         <|> assignStatement

manyStatements = do
    ls <- (sepBy1 statement' semiC)
    return $ if length ls == 1 then head ls else Many ls

whileParser :: Parser Statement
whileParser = wSpace >> statement



