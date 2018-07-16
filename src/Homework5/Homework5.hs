{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Homework5.Homework5 where

import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Homework5.ExprT as ExprT
import qualified Homework5.Parser as Parser
import qualified Homework5.StackVM as StackVM

-- ex1 start
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y
-- ex1 end

-- ex2 start
evalStr :: String -> Maybe Integer
evalStr = fmap eval . Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul
-- ex2 end

-- ex3 start
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
reify = id
-- ex3 end

-- ex4 start
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . flip mod 7 . max 0
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y
-- ex4 end

-- ex5 start
instance Expr StackVM.Program where
  lit = (:[]) . StackVM.PushI
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]
-- ex5 end

-- ex6 start
class HasVars a where
  var :: String -> a

data VarExprT = VarExprT String Integer deriving (Eq, Show)

instance Expr VarExprT where
  lit = VarExprT ""
  add (VarExprT _ x) (VarExprT _ y) = VarExprT "" $ x + y
  mul (VarExprT _ x) (VarExprT _ y) = VarExprT "" $ x * y

instance HasVars VarExprT where
  var = flip VarExprT 0

type VarExpr = M.Map String Integer -> Maybe Integer

instance HasVars VarExpr where
  var = M.lookup

instance Expr VarExpr where
  lit x _ = Just x
  add x y m =
    if Maybe.isNothing (x m) || Maybe.isNothing (y m)
    then Nothing
    else Just $ Maybe.fromJust (x m) + Maybe.fromJust (y m)
  mul x y m =
    if Maybe.isNothing (x m) || Maybe.isNothing (y m)
    then Nothing
    else Just $ Maybe.fromJust (x m) * Maybe.fromJust (y m)

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
-- ex6 end
