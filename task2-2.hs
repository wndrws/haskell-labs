-- Часть 1: свои реализации сверток
foldl' fun acc [] = acc
foldl' fun acc lst = fun (foldl' fun acc $ tail lst) (head lst)

foldr' fun acc [] = acc
foldr' fun acc lst = fun (foldr' fun acc $ init lst) (last lst)

-- Часть 2: реализации требуемых функций на основе своих сверток
map' :: (a -> b) -> [a] -> [b]
map' fun lst = foldl' (\acc x -> (fun x) : acc) [] lst 

flatMap' :: (a -> [b]) -> [a] -> [b]  
flatMap' fun lst = foldl' (\acc x -> (fun x) ++ acc) [] lst

concat' :: [a] -> [a] -> [a]  
concat' left right = foldl' (\acc x -> x : acc) right left

filter' :: (a -> Bool) -> [a] -> [a]  
filter' pred lst = foldl' (\acc x -> (if pred x then [x] else []) ++ acc) [] lst

maxBy' :: (a -> Integer) -> [a] -> a
maxBy' fun (h:t) = foldl' (\acc x -> if (fun x) > (fun acc) then x else acc) h t

minBy' :: (a -> Integer) -> [a] -> a
minBy' fun (h:t) = foldl' (\acc x -> if (fun x) < (fun acc) then x else acc) h t

reverse' :: [a] -> [a]
reverse' lst = foldr' (\acc x -> x : acc) [] lst

elementAt' :: Int -> [a] -> a
elementAt' idx lst 
    | idx < 0 || idx > (length lst) - 1 = error "Out of bounds"
    | otherwise = head $ snd $ 
        foldr' (
            \res@(cnt, acc) x -> 
                if cnt <= idx then (succ cnt, x : acc) else res
        ) (0, []) lst

-- Находит индекс первого вхождения элемента. Если элемента в списке нет, то возвращает -1.
indexOf' :: (Eq a) => a -> [a] -> Integer -- (в т.ч. String -> [String] -> Integer)
indexOf' val lst
    | not (val `elem` lst) = -1
    | otherwise = pred $ fst $ 
        foldr' (
            \res@(cnt, acc) x -> 
                if not (val `elem` acc) then (succ cnt, x : acc) else res
        ) (0, []) lst