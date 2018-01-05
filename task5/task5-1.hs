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

rhead :: ReverseList a -> a
rhead = view rheadLens