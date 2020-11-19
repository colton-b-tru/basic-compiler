import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.ParserCombinators.Parsec.Expr
import Control.Monad 

data Statement = LET Variable Expression
    | PRINT Expression 
    | INPUT String--INPUT String IDList 
    | FOR ID Expression Expression 
    | IF Expression Integer 
    | NEXT IDList 
    | RETURN
    | END 


data Expression = 
    -- AndExpr | OrExpr AndExpr Expression 
    -- | 
languageDef =
    emptyDef {Token.commentLine = "REM"
             , Token.identStart = upper
             , Token.reservedOpNames = [
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
             ]
    }



main = do
    print ("main")
        -- let lexer = Token.makeTokenParser languageDef
        --     identifier = Token.identifier lexer
        --     in print (identifier "A")