module Homework5.Homework5 where

import Homework5.ExprT
import Homework5.Parser

-- ex1 start
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
-- ex1 end

-- ex2 start
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end
