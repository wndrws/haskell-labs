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

emptyDeque :: Deque a -- Константная сложность
emptyDeque = Deque [] []

enqueueLeft :: Deque a -> a -> Deque a -- Константная сложность, т.к. функция (:) имеет сложность O(1)
enqueueLeft (Deque i o) x = Deque (x:i) o

enqueueRight :: Deque a -> a -> Deque a -- Константная сложность, т.к. функция (:) имеет сложность O(1)
enqueueRight (Deque i o) x = Deque i (x:o)

dequeueLeft :: Deque a -> (Deque a, a) -- Линейная сложность, т.к. использует функции halves и reverse
dequeueLeft (Deque [] []) = error "Deque is empty"
dequeueLeft (Deque [] [x]) = (emptyDeque, x)
dequeueLeft (Deque [] o) = let (lhalf, rhalf) = halves o in dequeueLeft $ Deque (reverse rhalf) lhalf
dequeueLeft (Deque (hi:ti) o) = (Deque ti o, hi)

dequeueRight :: Deque a -> (Deque a, a) -- Линейная сложность, т.к. использует функции halves и reverse
dequeueRight (Deque [] []) = error "Deque is empty"
dequeueRight (Deque [x] []) = (emptyDeque, x)
dequeueRight (Deque i []) = let (lhalf, rhalf) = halves i in dequeueRight $ Deque lhalf (reverse rhalf)
dequeueRight (Deque i (ho:to)) = (Deque i to, ho)

-- Функция для разделения списка одновременно с поиском его середины 
-- (быстрее, чем связка splitAt с вычислением длины списка с помощью length)
halves :: [a] -> ([a], [a]) -- Линейная сложность, а точнее O(N/2)
halves list = halve list list where
    halve :: [a] -> [a] -> ([a], [a])
    halve (x:tx) (_:_:tty) = let (dx, ddy) = halve tx tty in (x:dx, ddy)
    halve x _ = ([], x)
-- * т.к. при нечетном числе элементов в списке можно выбирать большей либо левую,
-- либо правую часть, то при вызове halves [x] можно получить как ([],[x]), так 
-- и ([x], []). Данная реализация halves дает первый вариант, т.е. ([],[x]).

{- Задание 7.3

Единственной дорогостоящей операцией является "перекидывание" элементов, происходящее при
опустошении одно из списков деки, которое возможно в функциях dequeueLeft и dequeueRight.
Операция "перекидывания" занимает O(N), т.к. включает в себя функцию разделения списка пополам,
работающую за O(N/2), и функцию reverse, которая применяется к одной из половин, т.е. еще O(N/2).

-- Метод физика

В качестве функции потенциала выберем P = n + d, где n - общее число элементов в деке, а d - разница
между количеством элементов левом и правом списках (т.е. модуль разности числа элементов в них).
Так как n >= 0 и d >= 0 для любых n и d, то такой потенциал всегда неотрицателен.

В таком случае дека вида Deque[][1,2,3,4] обладает большим потенциалом, чем Deque[1,2][3,4], что
отражает уменьшение потенциала при выполнении дорогостоящей операции "перекидывания" элементов.

Добавление элемента в деку с любой стороны занимает O(1) и либо не меняет потенциал (в случае, когда
добавление элемента производится в меньший список), либо увеличивает его на 2 (в противном случае).

Удаление элемента из деки без "перекидывания" занимает O(1) и так же либо не меняет P, либо уменьшает на 2.
Удаление с "перекидыванием" занимает O(n), и меняет потенциал следующим образом:
    delta_P = P_new - P_old = (n-1 + d_new) - (n + d_old) = (n-1 + 0..1) - (n + n - 1) = -n + const
(до переброса d_old равно n - 1, т.к. переброс происходит, когда из списка извлекается последний элемент,
а после d_new может быть равно либо 0, либо 1 - в зависимости от четности n, что не влияет на сложность)
Тогда амортизированная сложность a_deq равна:
    a_deq = c_deq + delta_P = n+1 + (-n + const) = const,
где c_deq - затраты времени на операцию "перекидывания" и удаления элемента.

Таким образом, рассмотренная структура Deque с данным набором операций имеет амортизированную сложность O(1).

-- Метод банкира

С учётом уже упомянутых выводов о временных затратах отдельных операций, сделаем вывод об амортизированной
сложности методом банкира (или бухгалтера):
- Пусть каждая операция добавления элемента в деку и удаления без "перекидывания" приносит нам $2. 
- Каждая операция удаления с "перекидыванием" стоит $N, которые мы уже имеем за счет добавления N
  элементов, а так как каждому перекидыванию (кроме, возможно, самого первого) предшествует floor(N/2)
  удалений без перекидывания, приносимые ими деньги компенсируют затраты на последующее удаление
  с "перекидыванием" (которое, заметим, будет происходить уже с меньшим числом элементов).

Таким образом, рассмотренная структура Deque с данным набором операций имеет амортизированную сложность O(1).

-}