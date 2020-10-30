-- don't be basic, parse some Basic 

main = do 
    let test = "LET A = 2"
    print (test)


data Value = Variable | Function | Constant -- or '(' <Expression> ')' but I'm not sure how to handle that 

data Statements =  Statement ':' Statements | Statement

data Constant = Integer | String  
--there's quite a few of these... but let's start with the pieces
--we need to parse foo.bas and test.bas 
data Statement = Let 

-- do we have to make a lexer? 


