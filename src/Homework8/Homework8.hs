{-# LANGUAGE NamedFieldPuns #-}

module Homework8.Homework8 where

import qualified Data.Tree as T

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
moreFun :: E.GuestList -> E.GuestList -> E.GuestList
moreFun lgl@(E.GL _ lFun) rgl@(E.GL _ rFun)
  | lFun > rFun = lgl
  | otherwise = rgl
-- ex1.3 end

-- ex2 start
treeFold :: (a -> [b] -> b) -> T.Tree a -> b
treeFold f T.Node { T.rootLabel = label, T.subForest = trees } =
  f label (map (treeFold f) trees)
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end
