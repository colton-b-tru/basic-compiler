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
data Statements =  Statement Statements | Statement
data Statement = END | FOR ID Expression Expression | FOR ID Expression Expression | GOTO Integer | GOSUB Integer 
                | IF Expression Integer | INPUT String IDList | Let Variable Expression | NEXT IDList
                | ON <Expression> GOTO <Integer List> | PRINT PrintList | PRINT Expression -- | PRINT TAB <Print list> 
                | REM {Printable}* | RETURN -- | Equal Variable Expression this one is like a let... 
instance Show Statement where 
    show (Let var expr) =  "LET " ++ (show var) ++ " = " ++ (show expr) 
    show (Print expr) = "Print " ++ (show expr)

-- grammar goes that massive cascade to end up at value ... I think we have to make all of those data types 
data Expression = Value | AddExpr Expression Op Expression | ID Char | ExprString String | Integer 
data AndExpr = NotExpr AndExpr | NotExpr
data NotExpr = Not CompareExpr | CompareExpr
data CompareExpr = Eq AddExpr CompareExpr | Angle AddExpr CompareExpr | Gt AddExpr CompareExpr 
                    | Gte AddExpr CompareExpr | Lt AddExpr CompareExpr | Lte AddExpr CompareExpr | AddExpr 
data AddExpr = Plus MultExpr AddExpr | Minus MultExpr AddExpr | MultExpr 
data MultExpr = Times NegateExpr AddExpr | Div NegateExpr AddExpr | NegateExpr 
data NegateExpr = PowerExpr -- | '-' PowerExpr don't know what this is 
data PowerExpr = Power Value PowerExpr | Value 
data Value = Variable | Function | Constant 
data Variable = ID -- | Array 
data ID = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Enum, Show)
-- <Function> ::= INT '(' <Expression> ')' | RND '(' <Expression> ')'
data Function = Inte Expression | Rnd Expression
data Constant = Integer | String



-- <value> ::= '(' <Expression> ')'
--            | Variable
--            | Function
--            | Constant
instance Show Expression where
    show (Value int) = show int

instance Show Variable where
    show (ID c) = show c


-- <Expression>  ::= <And Exp> OR <Expression>
--                 | <And Exp>

-- data Value = Variable |  Constant -- | Function | '(' <Expression> ')' but I'm not sure how to handle that 
-- data Variable = ID Char-- | Array (we'll use you later friend)
-- data Constant = Integer | ExprString String deriving Show




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
    return (AddExpr lExpr op rExpr)



-- ========== statement related parsing 
pStatement =
    letStatement P.<|>
    printStatement P.<|>


letStatement = do
    P.string "LET "
    var <- C.letter -- probably string is more appropriate
    P.string " = " -- not a fan but this will eat what i want 
    expr <- pExpression 
    return (Let (ID var)  expr)

printStatement = do 
    P.string "PRINT "
    expr <- pExpression
    return (Print expr)

forStatement = do 
    P.string "FOR "
    expr 

p :: String -> Statement 
assumeRight (Right r) = r
assumeRight (Left r) = error (show r) 
p string = assumeRight $ P.parse pStatement "OURerror" string




