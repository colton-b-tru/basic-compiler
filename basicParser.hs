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
    let  parseMe = "LET A = 2"
    print (p parseMe)

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
data Statement = END | FOR ID Expression Expression | GOTO Integer | GOSUB Integer 
                | IF Expression Integer  | INPUT1 String | LET Variable Expression | NEXT IDList
                | ONGOTO Expression IntList | PRINT Expression -- | PRINT TAB <Print list> 
                | RETURN 
                | INPUT String IDList
                -- | Equal Variable Expression this one is like a let... 
                -- | REM {Printable}* 
                -- | ON Expression GOTO Integer List
-- instance Show Statement where 
--     show (Let var expr) =  "LET " ++ (show var) ++ " = " ++ (show expr) 
--     show (PRINT expr) = "Print " ++ (show expr)

data PrintList = Comma Expression PrintList | Semi Expression PrintList | Expression 

-- grammar goes that massive cascade to end up at value ... I think we have to make all of those data types 
data Expression = OrExp AndExpr Expression  
                | NotExp CompareExp 
                | CompareExp AddExp CompareExp
                | AddExp MultExp AddExp 
                | MultExp NegateExp MultExp 
                | NegateExp PowerExp
                | PowerExp Value PowerExp
                | Value deriving (Show)
data AndExpr = AndExp NotExpr AndExpr | NotExpr deriving (Show) 
data NotExpr = Not CompareExp | CompareExpr deriving (Show) 
data CompareExp = Eq AddExp CompareExp
                    | Angle AddExp CompareExp 
                    | Gt AddExp CompareExp
                    | Gte AddExp CompareExp 
                    | Lt AddExp CompareExp 
                    | Lte AddExp CompareExp deriving (Show) 
data AddExp = Plus MultExp AddExp | Minus MultExp AddExp deriving (Show) 
data MultExp = Multiply NegateExp AddExp | Divide NegateExp AddExp deriving (Show)
data NegateExp = NegExpr PowerExp deriving (Show)
data PowerExp = PowerExpr0 Value PowerExp | PowerExpr1 Value deriving (Show) 
data Value = Var Variable deriving (Show)
data Variable = Variable ID deriving (Show)
data ID = Id Char deriving (Show)
data IntList = SngList Integer | List IntList deriving (Show) 
data IDList = IDComma ID IDList deriving (Show)


-- 

-- 
-- data Value = Variable 
--     | Function 
--     | ConstantInt 
--     | ConstantString deriving (Show) 
-- data Variable = ID deriving (Show) -- | Array deriving (Show) 
-- data ID = Id Char deriving (Show) -- A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Enum, Show)
--   -- 

-- -- <Function> ::= INT '(' <Expression> ')' | RND '(' <Expression> ')'
-- data Function = Inte Expression | Rnd Expression deriving (Show) 
-- -- data Constant = Integer | String deriving (Show) 
-- data ConstantInt = ConstantIntInstance Integer
-- data ConstantString = ConstantStringInstance [Char]

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
             , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                      , "<", ">", "and", "or", "not"
                                      ]
              }

-- https://wiki.haskell.org/Parsing_a_simple_imperative_language#Lexer had some really useful parsec pieces
arithOps = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
              , [Infix  (reservedOp "*"   >> return (Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Divide   )) AssocLeft]
              , [Infix  (reservedOp "+"   >> return (Plus    )) AssocLeft,
                 Infix  (reservedOp "-"   >> return (Minus   )) AssocLeft]
              ]

boolOps  = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
              , [Infix  (reservedOp "and" >> return (And     )) AssocLeft,
                 Infix  (reservedOp "or"  >> return (Or      )) AssocLeft]
            ]

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


 
        
pExpression = 
    do
        addExpr
    P.<|>
    do 
        compareExpr
    -- P.<|>    
    -- do
         


-- do we haveto cascade down all these to make sure we cover all types of nesting in arithmetic expressoins? 
addExpr = 
    do
    lExpr <- pExpression
    op' <- reservedOp
    rExpr <- pExpression 
    case op' of "+" -> return (AddExpr lExpr op' rExpr)
                "-" -> return (Minus lExpr op' rExpr)
                "*" -> return (Times lExpr op' rExpr)
                "/" -> return (Divide lExpr op' rExpr)

compareExpr = do 
    addExp <- addExpr 
    compOp <- reservedOp
    compExp <- compareExpr 
    case compOp of "=" -> return (Eq addExp compExp)
                   "<>" -> return (Angle addExp compExp)
                   ">" -> return (Gt addExp compExp)
                   ">=" -> return (Gte addExp compExp)
                   "<" -> return (Lt addExp compExp)
                   "<=" -> return (Lte addExp compExp)
    P.<|>
    do addExpr






-- ========== statement related parsing 
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
    reserved "="
    expr <- pExpression
    return (LET var expr)

printStatement = do 
    reserved "PRINT"
    expr <- pExpression
    return (PRINT expr)
-- also consider this print PRINT TAB '(' <Expression> ')' <Print list>

forStatement = do 
    reserved "FOR"
    id <- idExpression 
    P.string " = "
    expr0 <- pExpression
    P.string " TO "
    expr1 <- pExpression
    return (FOR id expr0 expr1)

gotoStatement = do 
    P.string "GOTO"
    lineNum <- number
    return (GOTO lineNum)

gosubStatement = do 
    P.string "GOSUB" 
    lineNum <- number
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
    P.<|>
    do
    reserved "INPUT"
    str <- (many1 alphaNum)
    semi
    idList <- pIDList
    return (INPUT str idList)

pIDList = do 
    id <- variable
    C.char ','
    idList <- pIDList
    return (IDComma id idList) 

    







p :: String -> Statement 
assumeRight (Right r) = r
assumeRight (Left r) = error (show r) 
p string = assumeRight $ P.parse pStatement "OURerror" string




