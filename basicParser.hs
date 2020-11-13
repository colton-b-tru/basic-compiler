-- don't be basic, parse some Basic 
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.ParserCombinators.Parsec.Expr
import Control.Monad 

main = do
    -- let file = "test.bas"
    -- wordBank <- readFile file
    let  parseMe = "LET A = 2"
    print (p parseMe)

--there's quite a few of these... but let's start with the pieces
--we need to parse foo.bas and test.bas 
--ultimately what are we parsing? In Scheme it was Sexpr. I think here it's Statements 

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
                | IF Expression Integer | INPUT String IDList | Let Variable Expression | NEXT IDList
                | ONGOTO Expression IntList | PRINT Expression -- | PRINT TAB <Print list> 
                | RETURN 
                -- | Equal Variable Expression this one is like a let... 
                -- | REM {Printable}* 
                -- | ON Expression GOTO Integer List
instance Show Statement where 
    show (Let var expr) =  "LET " ++ (show var) ++ " = " ++ (show expr) 
    show (PRINT expr) = "Print " ++ (show expr)

data PrintList = Comma Expression PrintList | Semi Expression PrintList | Expression 

-- grammar goes that massive cascade to end up at value ... I think we have to make all of those data types 
data Expression = OrExp AndExpr Expression | AndExpr        --- Value | AddExpr Expression Op Expression | ID | Constant
data AndExpr = AndExp NotExpr AndExpr | NotExpr
data NotExpr = Not CompareExpr | CompareExpr
data CompareExpr = Eq AddExpr CompareExpr | Angle AddExpr CompareExpr | Gt AddExpr CompareExpr 
                    | Gte AddExpr CompareExpr | Lt AddExpr CompareExpr | Lte AddExpr CompareExpr | AddExpr 
data AddExpr = Plus MultExpr AddExpr | Minus MultExpr AddExpr | MultExpr 
data MultExpr = Times NegateExpr AddExpr | Div NegateExpr AddExpr | NegateExpr 
data NegateExpr = PowerExpr -- | '-' PowerExpr don't know what this is 
data PowerExpr = Power Value PowerExpr | Value 
data Value = Variable | Function | Constant 
data Variable = ID -- | Array 
data ID =  ID Char-- A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Enum, Show)
data IDList = IDComma ID IDList -- | ID 
data IntList = Integer | Integer IntList
-- <Function> ::= INT '(' <Expression> ')' | RND '(' <Expression> ')'
data Function = Inte Expression | Rnd Expression
data Constant = Integer | String


instance Show Expression where
    show (Value int) = show int

instance Show Variable where
    show (ID c) = show c




number :: GenParser Char st Sexpr
number = do {n <- (many1 digit); spaces; return $ Number (read n :: Int)}
-- ================= expression related parsing 
num = do
    num' <- P.many1 P.digit
    return $ Value (read num' :: Integer)
op = do 
        C.char '+' 
        return Plus 
    P.<|>
    do
        C.char '-' 
        return Minus
    P.<|>
    do
        C.char '*'
        return Times 
    P.<|>
    do
        C.char '/' 
        return Div 

var = do 
        varName <- C.upper 
        return (ID varName) -- or is this ID varName ? 
        
pExpression = 
    do
        num
    P.<|>
    do 
        var 
    P.<|>    
    do
        addExpr 


-- do we haveto cascade down all these to make sure we cover all types of nesting in arithmetic expressoins? 
addExpr = 
    do
    lExpr <- pExpression
    op' <- op
    rExpr <- pExpression 
    case op' of Plus return (AddExpr lExpr op rExpr)







-- ========== statement related parsing 
pStatement =
    letStatement P.<|>
    printStatement P.<|>
    forStatement P.<|>
    gotoStatement P.<|>
    gosubStatement P.<|>
    ifThenStatement P.<|>
    inputStatement P.<|>
    nextStatement P.<|>
    onGotoStatement P.<|>
    remStatement P.<|>
    returnStatement P.<|>
    -- var = expr
    

letStatement = do
    P.string "LET "
    var <- C.letter -- probably string is more appropriate
    P.string " = " -- not a fan but this will eat what i want 
    expr <- pExpression 
    return (Let (ID var)  expr)

printStatement = do 
    P.string "PRINT "
    expr <- pExpression
    return (PRINT expr)
-- also consider this print PRINT TAB '(' <Expression> ')' <Print list>

forStatement = do 
    P.string "FOR "
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
    P.string "IF "
    expr <- pExpression 
    P.string " THEN "
    lineNum <- number 
    return (IF expr lineNum)

-- INPUT String ';' <ID List>
inputStatement = do 
    P.string "INPUT " 
    string <- P.string 
    C.char ';'
    idList <- pIDList
    return (INPUT string idList)
-- NEXT <ID List>
nextStatement = do 
    P.string "NEXT "
    idList <- pIDList
    return (NEXT idList)
-- ON <Expression> GOTO <Integer List>
onGotoStatement = do 
    P.string "ON "
    expr <- pExpression 
    P.string " GOTO "
    intList <- pIntList 
    return (ONGOTO expr intlist)

returnStatement = do 
    P.string "RETURN"
    return (RETURN)



p :: String -> Statement 
assumeRight (Right r) = r
assumeRight (Left r) = error (show r) 
p string = assumeRight $ P.parse pStatement "OURerror" string




