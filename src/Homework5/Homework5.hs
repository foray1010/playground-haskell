module Homework5.Homework5 where

import Homework5.ExprT

-- ex1 start
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
-- ex1 end

-- ex2 start
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end
