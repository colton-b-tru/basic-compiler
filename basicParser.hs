-- don't be basic, parse some Basic 
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Control.Monad 

main = do
    -- let file = "test.bas"
    -- wordBank <- readFile file
    let test = "LET A = 2"
    case (parse test) of
        (Left _)-> print "failed"
        (Right r) print r 

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
data ID = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Enum, show)
data Statement = Let Variable Expression

instance Show Statement where 
    show (Let var expr) =  "LET " ++ (show var) ++ " = " ++ (show expr) 

instance Show Variable where
    show (ID c) = show c
instance Show Expression where
    
data Statements =  Statement ':' Statements | Statement

-- <Expression>  ::= <And Exp> OR <Expression>
--                 | <And Exp>
data Expression = Value Integer

data Variable = ID Char-- | Array (we'll use you later friend) 

data Value = Variable | Function | Constant -- | '(' <Expression> ')' but I'm not sure how to handle that 


data Constant = Integer | String deriving Show



pExpression = do
    num <- int 
    return num 
    -- <|> just taking care of num for now 
    var <- char -- probably string is more appropriate
    string " = " -- not a fan but this will eat what i want 
    expr <- pExpression 
    return (Let var expr)


