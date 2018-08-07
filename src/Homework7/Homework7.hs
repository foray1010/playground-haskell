module Homework7.Homework7 where

import qualified Homework7.Sized as Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- ex1 start
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y
-- ex1 end

-- ex2 start
indexJ :: (Sized.Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r)
  | i < 0 || i >= size = Nothing
  | i < sizeL = indexJ i l
  | otherwise = indexJ (i - sizeL) r
  where
    getSize = Sized.getSize . Sized.size
    size = getSize m
    sizeL = getSize . tag $ l
indexJ _ _ = Nothing
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end

-- ex7 start
-- ex7 end
