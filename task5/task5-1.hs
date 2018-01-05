--{-# LANGUAGE RankNTypes #-}

import Control.Lens

import Term
import ReverseList

-- Линза, фокусирующаяся на голове перевернутого списка (для R[1,2,3] - это 3)
rheadLens :: Lens' (ReverseList a) a
rheadLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons _ elem) = elem
    setter RNil _ = errorEmptyList
    setter (RCons rlst elem) newelem = RCons rlst newelem

errorEmptyList = error "Cannot focus on an empty ReverseList"

-- Примеры для rheadLens
rhead :: ReverseList a -> a
rhead = view rheadLens

rlist = fromList [1..5]
rheadLens_Example1 = rhead rlist -- Выведет голову списка R[1..5], то есть число 5
rheadLens_Example2 = over rheadLens (*2) rlist -- Выведет список с удвоенной головой, т.е. числом 10 вместо 5
rheadLens_Example3 = fromList [(x,y) | x <- [1..3], y <- ['a'..'z']] ^. rheadLens . _2 -- Композиция линз (выведет 'z')