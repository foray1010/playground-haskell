{-# LANGUAGE NamedFieldPuns #-}

module Homework8.Homework8 where

import qualified Homework8.Employee as E

-- ex1.1 start
glCons :: E.Employee -> E.GuestList -> E.GuestList
glCons emp@E.Emp { E.empFun = fun } (E.GL empl totalFun) =
  E.GL (empl ++ [emp]) (fun + totalFun)
-- ex1.1 end

-- ex1.2 start
instance Semigroup E.GuestList where
  (<>) (E.GL lEmpl lFun) (E.GL rEmpl rFun) = E.GL (lEmpl ++ rEmpl) (lFun + rFun)

instance Monoid E.GuestList where
  mempty = E.GL [] 0
-- ex1.2 end

-- ex1.3 start
-- ex1.3 end

-- ex2 start
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end
