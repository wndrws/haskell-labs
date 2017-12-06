-- Часть 1: свои реализации сверток
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' fun acc [] = acc
foldl' fun acc (h:t) = foldl' fun (fun acc h) t

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' fun acc [] = acc
foldr' fun acc (h:t) = fun h (foldr' fun acc t)

-- Часть 2: реализации требуемых функций на основе своих сверток
map' :: (a -> b) -> [a] -> [b]
map' fun = foldl' (\ acc x -> fun x : acc) [] 

flatMap' :: (a -> [b]) -> [a] -> [b]  
flatMap' fun = foldl' (\ acc x -> fun x ++ acc) []

concat' :: [a] -> [a] -> [a]  
concat' left right = foldr' (:) right left

filter' :: (a -> Bool) -> [a] -> [a]  
filter' pred = foldl' (\ acc x -> [x | pred x] ++ acc) []

maxBy' :: (a -> Integer) -> [a] -> a
maxBy' fun (h:t) = foldl' (\acc x -> if fun x > fun acc then x else acc) h t

minBy' :: (a -> Integer) -> [a] -> a
minBy' fun (h:t) = foldl' (\acc x -> if fun x < fun acc then x else acc) h t

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

elementAt' :: Int -> [a] -> a
elementAt' idx lst 
    | idx < 0 || idx > length lst - 1 = error "Out of bounds"
    | otherwise = head $ snd $ 
        foldl' (
            \res@(cnt, acc) x -> 
                if cnt <= idx then (succ cnt, x : acc) else res
        ) (0, []) lst

-- Находит индекс первого вхождения элемента. Если элемента в списке нет, то возвращает -1.
indexOf' :: (Eq a) => a -> [a] -> Integer -- (в т.ч. String -> [String] -> Integer)
indexOf' val lst = if valFound then index else -1 where 
    (index, valFound) = foldl' (\(idx, isFound) x -> 
        if x == val then (idx, True) else (if isFound then idx else succ idx, isFound)
        ) (0, False) lst