--{-# LANGUAGE RankNTypes #-}

import Control.Lens

import Term
import ReverseList

errorEmptyList = error "Cannot focus on an empty ReverseList"

-- Линза, фокусирующаяся на голове перевернутого списка (для R[1,2,3] - это 3)
rheadLens :: Lens' (ReverseList a) a
rheadLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons _ elem) = elem
    setter RNil _ = errorEmptyList
    setter (RCons rlst elem) newelem = RCons rlst newelem

-- Примеры для rheadLens
rhead :: ReverseList a -> a
rhead = view rheadLens

exampleRList = fromList [1..5]
rheadLens_Example1 = rhead exampleRList -- Выведет голову списка R[1..5], то есть число 5
rheadLens_Example2 = over rheadLens (*2) exampleRList -- Выведет список с удвоенной головой, т.е. числом 10 вместо 5
rheadLens_Example3 = fromList [(x,y) | x <- [1..3], y <- ['a'..'z']] ^. rheadLens . _2 -- Композиция линз (выведет 'z')

-- Линза, фокусирующаяся на хвосте перевернутого списка (для R[1,2,3] - это R[1,2])
rtailLens :: Lens' (ReverseList a) (ReverseList a)
rtailLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons rlst _) = rlst
    setter RNil _ = errorEmptyList
    setter (RCons rlst elem) newrlst = RCons newrlst elem

-- Примеры для rtailLens
rtail :: ReverseList a -> ReverseList a
rtail = view rtailLens

rtailLens_Example1 = rtail exampleRList -- Выведет хвост списка R[1..5], то есть R[1,2,3,4]
rtailLens_Example2 = set rtailLens (fromList [42,43]) exampleRList -- Выведет список R[42,43,5]

