{- Задание 7.1
 Для реализации функций двусторонней очереди на основе структуры, рассмотренной на лекции,
 достаточно предусмотреть переброс элеменов не только из стека входящих в стек исходящих,
 но и наоборот, причем перебрасывать лучше не все элементы, а только половину (см. п. 7.3).
-}

-- Задание 7.2
data Deque a = Deque [a] [a]

instance (Show a) => Show (Deque a) where
    show (Deque [] []) = "Deque[][]"
    show (Deque i o) = "Deque" ++ show i ++ show (reverse o)

emptyDeque :: Deque a
emptyDeque = Deque [] []

enqueueLeft :: Deque a -> a -> Deque a
enqueueLeft (Deque i o) x = Deque (x:i) o

enqueueRight :: Deque a -> a -> Deque a
enqueueRight (Deque i o) x = Deque i (x:o)

dequeueLeft :: Deque a -> (Deque a, a)
dequeueLeft (Deque [] []) = error "Deque is empty"
dequeueLeft (Deque [] [x]) = (emptyDeque, x)
dequeueLeft (Deque [x] o) = (Deque newi newo, x) where
    newi = reverse . snd $ hh
    newo = fst hh
    hh = halves o
dequeueLeft (Deque (hi:ti) o) = (Deque ti o, hi)

dequeueRight :: Deque a -> (Deque a, a)
dequeueRight (Deque [] []) = error "Deque is empty"
dequeueRight (Deque [x] []) = (emptyDeque, x)
dequeueRight (Deque i [x]) = (Deque newi newo, x) where
    newi = fst hh
    newo = reverse . snd $ hh
    hh = halves i
dequeueRight (Deque i (ho:to)) = (Deque i to, ho)

-- Функция для разделения списка одновременно с поиском его середины 
-- (быстрее, чем связка splitAt с вычислением длины списка с помощью length)
halves :: [a] -> ([a], [a])
halves list = halve list list where
    halve :: [a] -> [a] -> ([a], [a])
    halve (x:tx) (_:_:tty) = let (dx, ddy) = halve tx tty in (x:dx, ddy)
    halve x _ = ([], x)
-- * т.к. при нечетном числе элементов в списке можно выбирать большей либо левую,
-- либо правую часть, то при вызове halves [x] можно получить как ([],[x]), так 
-- и ([x], []). Данная реализация halves дает первый вариант, т.е. ([],[x]).

viewLeft :: Deque a -> a
viewLeft deque = let (_, x) = dequeueLeft deque in x

viewRight :: Deque a -> a
viewRight deque = let (_, x) = dequeueRight deque in x

{- Задание 7.3
 
-}