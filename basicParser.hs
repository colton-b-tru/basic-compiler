-- don't be basic, parse some Basic 
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.ParserCombinators.Parsec.Expr
import Control.Monad 
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

        
main = do
    -- let file = "test.bas"
    -- fileContent <- readFile file
    print "TEST"

--Lance Provided Grammar 
-- <Statement>   ::= DIM <Array List>
--                 | END
--                 | FOR ID '=' <Expression> TO <Expression>
--                 | FOR ID '=' <Expression> TO <Expression> STEP <Expression>
--                 | GOTO Integer
--                 | GOSUB Integer
--                 | IF <Expression> THEN Integer
--                 | INPUT String ';' <ID List>
--                 | LET <Variable> '=' <Expression>
--                 | NEXT <ID List>
--                 | ON <Expression> GOTO <Integer List>
--                 | PRINT <Print list>
--                 | PRINT TAB '(' <Expression> ')' <Print list>
--                 | REM {Printable}*
--                 | RETURN
--                 | <Variable> '=' <Expression>

-- I think we can take care of these in the cascade , data Op = Plus | Minus | Times | Div 
-- data Statements =  Statement Statements | Statement
data Statement = END | FOR Expression Expression Expression | GOTO Integer | GOSUB Integer 
                | IF Expression Integer  | INPUT1 String | LET Expression Expression | NEXT Expression
                | ONGOTO Expression Expression | PRINT Expression -- | PRINT TAB <Print list> 
                | RETURN 
                | INPUT String [Expression]
                | Sequence [Statement]
                -- | Equal Variable Expression this one is like a let... 
                -- | REM {Printable}* 
                -- | ON Expression GOTO Integer List
-- instance Show Statement where 
--     show (Let var expr) =  "LET " ++ (show var) ++ " = " ++ (show expr) 
--     show (PRINT expr) = "Print " ++ (show expr)

--data PrintList = Comma Expression PrintList | Semi Expression PrintList | Expression 

data Expression = NotExp Expression 
                | ArithExp Op Expression Expression
                | BoolExp Op Expression Expression
                | CompareExp Op Expression Expression 
                | NegateExp Expression 
                | PowerExp Expression Expression --left expr is a value... 
                | VarValue Char 
                | FuncInt Expression 
                | FuncRnd Expression 
                | ConstInt Integer 
                | ConstStr String
                | ID Char
                | List Expression --can come in the form of , ; 
data Op = Plus | Minus | Multiply | Divide | Gt | Gte | Lt | Lte | And | Or | Not | Angle | Equal deriving (Show) 

-- grammar goes that massive cascade to end up at value ... I think we have to make all of those data types 
-- data Expression = OrExp AndExp Expression  
--                 | NotExp CompareExp 
--                 | CompareExp AddExp CompareExp
--                 | AddExp MultExp AddExp 
--                 | MultExp NegateExp MultExp 
--                 | NegateExp PowerExp
--                 | PowerExp Value PowerExp
--                 | Value deriving (Show)
-- data AndExp = AndExpr NotExp AndExp deriving (Show)  
-- data NotExp = Not CompareExp deriving (Show) 
-- data CompareExp = Eq AddExp CompareExp
--                     | Angle AddExp CompareExp 
--                     | Gt AddExp CompareExp
--                     | Gte AddExp CompareExp 
--                     | Lt AddExp CompareExp 
--                     | Lte AddExp CompareExp deriving (Show) 
-- data AddExp = Plus MultExp AddExp | Minus MultExp AddExp deriving (Show) 
-- data MultExp = Multiply NegateExp MultExp | Divide NegateExp MultExp deriving (Show)
-- data NegateExp = NegExpr PowerExp deriving (Show)
-- data PowerExp = PowerExpr0 Value PowerExp | PowerExpr1 Value deriving (Show) 
-- data Value = Var Variable | ConstantInt Integer | ConstantStr String deriving (Show) -- | Func Function | C
-- data Variable = Variable ID deriving (Show)
-- data ID = Id Char deriving (Show)
-- data IntList = SngList Integer | List IntList deriving (Show) 
-- data IDList = IDComma ID IDList deriving (Show)
--data ConstantInt = ConstantIntInstance Integer
--data ConstantString = ConstantStringInstance [Char]


-- -- <Function> ::= INT '(' <Expression> ')' | RND '(' <Expression> ')'
-- data Function = Inte Expression | Rnd Expression deriving (Show) 

-- instance Show Expression where
--     show Value = show int

-- instance Show Variable where
--     show (ID c) = show c



-- ++++++++= LEXER STUFF
languageDef =
    emptyDef {Token.commentLine = "REM"
             , Token.identStart = upper
             , Token.reservedNames = [
                 "PRINT"
                 , "LET"
                 , "END"
                 , "INPUT"
                 , "INT"
                 , "FOR"
                 , "NEXT"
                 , "RND"
                 , "+"
                 , "*"
                 , "TO"
                 , "IF"
                 , "THEN"
                 , "NEXT"
                 ,"END"
                 ,"RETURN"
               ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "=", "^"
                                      , "<", ">", ">=", "<=", "<>", "AND", "OR", "NOT"
                                      ]
              }


lexer = Token.makeTokenParser languageDef 

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis 
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
variable = do 
        varName <- C.upper 
        return (ID varName) -- or is this ID varName ?

aExpression = buildExpressionParser arithOps arithExpressions
bExpression = buildExpressionParser boolOps boolExpressions 

-- https://wiki.haskell.org/Parsing_a_simple_imperative_language#Lexer had some really useful parsec pieces
arithOps = [    [Prefix (reservedOp "-"      >> return (NegateExp      )) ]
              , [Infix  (reservedOp "*"   >> return (ArithExp Multiply )) AssocLeft,
                Infix  (reservedOp "/"    >> return (ArithExp Divide   )) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (ArithExp Plus     )) AssocLeft,
                 Infix  (reservedOp "-"   >> return (ArithExp Minus    )) AssocLeft]
              ]

boolOps  = [ [Prefix (reservedOp "NOT"    >> return (NotExp     ))         ]
              , [Infix  (reservedOp "AND" >> return (BoolExp And     )) AssocLeft,
                 Infix  (reservedOp "OR"  >> return (BoolExp Or      )) AssocLeft]
            ]

arithExpressions = parens aExpression
    P.<|> liftM ConstStr identifier -- this might have to be variable   
    P.<|> liftM ConstInt integer

boolExpressions = parens bExpression P.<|>
    do
        arithExp0 <- aExpression
        op <- relation 
        arithExp1 <- aExpression
        return $ CompareExp op arithExp0 arithExp1

relation =    (reservedOp ">"  >> return Gt)
        P.<|> (reservedOp "<"  >> return Lt)
        P.<|> (reservedOp ">=" >> return Gte)
        P.<|> (reservedOp "<=" >> return Lte)
        P.<|> (reservedOp "<>" >> return Angle)
        P.<|> (reservedOp "=" >> return Equal)

        
pExpression = 
    do
    aExpression 
    P.<|>
    do 
    bExpression 





-- ========== statement related parsing 
statement = parens statement 
    P.<|> sequenceStatement

sequenceStatement = do
    list <- (sepBy1 pStatement semi)
    return $ if length list == 1 then head list else Sequence list
    

pStatement =
    letStatement P.<|>
    printStatement P.<|>
    forStatement P.<|>
    gotoStatement P.<|>
    gosubStatement P.<|>
    ifThenStatement P.<|>
    endStatement P.<|>
    inputStatement P.<|>
    -- nextStatement P.<|>
    -- onGotoStatement P.<|>
    -- remStatement P.<|>
    returnStatement
    -- var = expr
    

letStatement = do
    reserved "LET"
    var <- variable
    reservedOp "="
    expr <- pExpression
    return (LET var expr)

printStatement = do 
    reserved "PRINT"
    expr <- pExpression
    return (PRINT expr)
-- also consider this print PRINT TAB '(' <Expression> ')' <Print list>

forStatement = do 
    reserved "FOR"
    expr0 <- pExpression
    reservedOp "="
    expr1 <- pExpression
    reserved "TO"
    expr2 <- pExpression
    return (FOR expr0 expr1 expr2)

gotoStatement = do 
    reserved "GOTO"
    lineNum <- integer
    return (GOTO lineNum)

gosubStatement = do 
    reserved "GOSUB" 
    lineNum <- integer
    return (GOSUB lineNum)

ifThenStatement = do 
    reserved "IF"
    exp <-  pExpression-- 
    reserved "THEN"
    lineNum <- integer 
    return (IF exp lineNum)

--INPUT String ';' <ID List>
inputStatement = do 
    reserved "INPUT"
    str <- (many1 alphaNum) --something that can pull out any string
    return (INPUT1 str)
    -- P.<|>
    -- do
    -- reserved "INPUT"
    -- str <- (many1 alphaNum)
    -- semi
    -- idList <- pIDList
    -- return (INPUT str idList)

returnStatement = do 
    reserved "RETURN"
    return (RETURN)

endStatement = do 
    reserved "END"
    return END



    







p :: String -> Statement 
assumeRight (Right r) = r
assumeRight (Left r) = error (show r) 
p string = assumeRight $ P.parse pStatement "OURerror" string




