module Lib (doDifferentialRepl) where

import Text.Parsec
import qualified Text.Parsec.Token as TokenSymbols
import Text.Parsec.Language (haskellDef)

-- AST --------------------------------------------------------------------------------
data Expression = Constant Integer
                | Variable String Integer
                | Mult Expression Expression
                | Plus Expression Expression deriving (Show, Eq)

{-
--ASTの例

x^2 + 3*x + 1
example1 = (Plus (Variable "x" 2) (Plus (Mult (Constant 3) (Variable "x" 1)) (Constant 1)))

10*x^4 + 2*x + 3 + 4
example2 = (Plus (Mult (Variable "x" 4) (Constant 10))
            (Plus (Mult (Variable "x" 1) (Constant 2))
             (Plus (Constant 3) (Constant 4))))
-}

{-
BNF :: (こんな式を想定 : 4*x^2 + 3x + 2)
pexp ::= mexp | mexp "+" pexp   // addition
mexp ::= texp | texp "*" mexp   // multiplication
texp ::= "x" "^" n | "x" | n    // term
n    ::= natural number
-}

-- Parser --------------------------------------------------------------------------------

lexer  = TokenSymbols.makeTokenParser(haskellDef)
number = TokenSymbols.natural lexer
symbol = TokenSymbols.symbol  lexer

parseVaribale      = do {symbol "x"; variable <- parseVariablePower; return variable}
parseVariablePower = (do {symbol "^"; num <- number; return (Variable "x" num)}
                      <|> do { return (Variable "x" 1) })

parseTexp          = (do {num <- number; return (Constant num)}
                      <|> do {variable <- parseVaribale; return variable})

parseMexp          = do {texp <- parseTexp; rexp <- parseMexpStar texp; return rexp}
parseMexpStar texp = (do {symbol "*"; mexp <- parseMexp; return (Mult texp mexp)}
                      <|> do return texp)

parsePexp          = do {mexp <- parseMexp; rexp <- parsePexpStar mexp; return rexp}
parsePexpStar mexp = (do {symbol "+"; pexp <- parsePexp; return (Plus mexp pexp)}
                      <|> do return mexp)

parseExpEof           = do { exp <- parsePexp; eof; return exp}

parseExp input = do exp <- (parse parseExpEof "input" input)
                    return exp

-- differential --------------------------------------------------------------------------------

-- differential caliculator
diff :: Expression -> String -> Expression
diff (Constant _) variable      = (Constant 0)
diff (Variable name n) variable = if name == variable then (Mult (Constant n) (Variable name (n - 1))) else (Constant 0)
diff (Mult left right) variable = (Plus (Mult (diff left variable) right) (Mult left (diff right variable)))
diff (Plus left right) variable = (Plus (diff left variable) (diff right variable))

-- simplifier
simplify' :: Expression -> Expression
simplify' const@(Constant _) = const
simplify' (Variable x 0) = (Constant 1)
simplify' var@(Variable _ _) = var
simplify' (Mult _ (Constant 0)) = (Constant 0)
simplify' (Mult (Constant 0) _) = (Constant 0)
simplify' (Mult (Constant n) (Constant m)) = (Constant (n * m))
simplify' (Mult (Mult (Constant n) var@(Variable _ _)) (Constant m)) = (Mult (Constant (n * m)) var)
simplify' (Mult left right) = (Mult (simplify' left) (simplify' right))
simplify' (Plus (Constant 0) expression) = (simplify' expression)
simplify' (Plus expression (Constant 0)) = (simplify' expression)
simplify' (Plus (Constant n) (Constant m)) = (Constant (n + m))
simplify' (Plus expression1 expression2) = (Plus (simplify' expression1) (simplify' expression2))

simplify expression = fix expression expression
  where
    fix expression before = if (before == after) then after else fix after after
      where
        after = (simplify' expression)

-- expression visualizer
visualize (Constant n) = (show n)
visualize (Variable x 1) = x
visualize (Variable x n) = x ++ "^" ++ (show n)
visualize (Mult c@(Constant _) exp) = (visualize c) ++ "*" ++ (visualize exp)
visualize (Mult exp c@(Constant _)) = (visualize c) ++ "*" ++ (visualize exp)
visualize (Plus a b) = (visualize a) ++ " + " ++ (visualize b)


-- Main Process --------------------------------------------------------------------------------

-- Parse Expression & Differential Calculation
evalDifferential :: String -> String
evalDifferential line = case parseExp line of
                          Right exp -> visualize $ simplify $ diff exp "x"
                          Left _    ->  "(´・_・`) < そんな名前の式知らない！"

-- Read Eval Print Loop
doDifferentialRepl :: IO ()
doDifferentialRepl =
  let loop = do
        line <- getLine
        if line == "exit" then do
          putStrLn "Bye! (´・_・`)/"
        else do putStrLn $ evalDifferential line
                loop
  in do loop
