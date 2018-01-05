{-# LANGUAGE RankNTypes #-}

import Control.Lens
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

-- Линза, фокусирующаяся на last перевернутого списка (для R[1,2,3] - это 1)
rlastLens :: Lens' (ReverseList a) a
rlastLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons RNil elem) = elem
    getter (RCons rlst _) = getter rlst
    setter RNil _ = errorEmptyList
    setter (RCons RNil elem) newelem = RCons RNil newelem
    setter (RCons rlst elem) newelem = RCons (setter rlst newelem) elem

-- Примеры для rlastLens
rlast :: ReverseList a -> a
rlast = view rlastLens

rlastLens_Example1 = rlast exampleRList -- Выведет last списка R[1..5], то есть число 1
rlastLens_Example2 = fmap show exampleRList & rlastLens %~ (++ " - this is the last") -- Укажет на last

-- Линза, фокусирующаяся на init перевернутого списка (для R[1,2,3] - это R[2,3])
rinitLens :: Lens' (ReverseList a) (ReverseList a)
rinitLens = lens getter setter where
    getter RNil = errorEmptyList
    getter (RCons RNil _) = RNil
    getter (RCons rlst elem) = RCons (getter rlst) elem
    setter RNil _ = errorEmptyList
    setter (RCons RNil _) newrlst = RCons (rtail newrlst) (rhead newrlst)
    setter (RCons rlst _) newrlst = RCons (RCons RNil (rlast rlst) `rconcat` rtail newrlst) (rhead newrlst)

-- Примеры для rinitLens
rinit :: ReverseList a -> ReverseList a
rinit = view rinitLens

rinitLens_Example1 = rinit exampleRList -- Выведет init списка R[1..5], то есть R[2,3,4,5]
rinitLens_Example2 = exampleRList & rinitLens .~ rtail exampleRList -- Заменит init на tail

-- Линза, фокусирующаяся на элементе перевернутого списка по его индексу (для R[1,2,3] элемент с индексом 0 - это 3)
rindexLens :: Int -> Lens' (ReverseList a) a
rindexLens index = if index < 0 then error "Negative index" else lens getter setter where
    getter RNil = errorEmptyList
    getter rlist = rget rlist index where 
        rget :: ReverseList a -> Int -> a
        rget (RCons _ elem) 0 = elem
        rget (RCons RNil _) _ = error "Index out of bounds"
        rget (RCons rlst _) idx = rget rlst $ pred idx
    setter RNil _ = errorEmptyList
    setter rlist newelem = rset rlist newelem index where
        rset :: ReverseList a -> a -> Int -> ReverseList a
        rset (RCons rlst _) new 0 = RCons rlst new
        rset (RCons RNil _) _ _ = error "Index out of bounds"
        rset (RCons rlst elem) new idx = rset rlst new (pred idx) `rconcat` RCons RNil elem

-- Примеры для rindexLens
rindex :: ReverseList a -> Int -> a
rindex rlist idx = rlist ^. rindexLens idx

rindexLens_Example1 = exampleRList `rindex` 1 -- Вывдет число 4 из списка R[1..5]
rindexLens_Example2 = set (rindexLens 1) 100500 exampleRList -- Вернет список R[1,2,3,100500,5]
rindexLens_Example3 = exampleRList & rindexLens 4 %~ negate -- Вернет список R[-1,2,3,4,5]