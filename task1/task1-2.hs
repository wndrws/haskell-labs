-- 1. Наибольший общий делитель двух чисел.
gcd' x y
    | x == 0 && y == 0 = error "GCD undefined"
    | otherwise = let a = abs x; b = abs y in
    if a*b == 0 then a+b else gcd' b (a `mod` b)

-- 2. Существует ли натуральное число, являющееся квадратом другого числа, между двумя заданными целыми числами?
anySquaresBetween :: Int -> Int -> Bool
anySquaresBetween a b -- Рассматриваем оба конца отрезка включительно
    | a < 0 && b < 0 = False
    | a > b = anySquaresBetween b a
    | otherwise = any isSquare [a .. b]

isSquare :: Int -> Bool
isSquare x 
    | x  < 0 = False 
    | x == 0 = True
    | x  > 0 = x `elem` scanl1 (+) lst 
        where lst = take (ceiling.sqrt $ fromIntegral x) [1,3..]
--    | x  > 0 = x `elem` scanl1 (+) [1,3..x]


-- 3. Возведение целого числа в целую степень за логарифмическое время.
(***) :: Int -> Int -> Double
_ *** 0 = 1
0 *** _ = 0
a *** 1 = fromIntegral a
1 *** _ = 1
a *** b
    | b < 0 = 1 / a *** (-b)
    | b > 1 = x*x*r where x = a *** (b `div` 2); r = a *** (b `mod` 2)