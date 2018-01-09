data Dyn = 
      Fun (Dyn -> Dyn)
    | Num Integer
    | Str String
    | List [Dyn]
    | Void

instance Show Dyn where
    show Void = "()"
    show (Num int) = show int
    show (Str str) = show str
    show (List lst) = show lst
    show (Fun _) = "Function"

instance Eq Dyn where
    Void == Void = True
    (Num l) == (Num r) = l == r
    (Str l) == (Str r) = l == r
    (List l) == (List r) = l == r
    (Fun _) == (Fun _) = error "Cannot compare functions"
    _ == _ = False -- Пусть сравнение разных типов считается допустимым и всегда возвращает False

instance Ord Dyn where
    Void <= Void = True
    (Num l) <= (Num r) = l <= r
    (Str l) <= (Str r) = l <= r
    (List l) <= (List r) = l <= r
    (Fun _) <= (Fun _) = error "Cannot compare functions"
    _ <= _ = False

showType :: Dyn -> String
showType Void = "Void"
showType (Num _) = "Num"
showType (Str _) = "Str"
showType (List _) = "List"
showType (Fun _) = "Fun"

errorUnary op a = error $ "Cannot perform " ++ show op ++ " on " ++ showType a
errorBinary op a b = error $ "Cannot perform " ++ show op ++ " on " ++ showType a ++ " and " ++ showType b

instance Num Dyn where
    (Num l) + (Num r) = Num (l + r)
    l + r = errorBinary "(+)" l r

    (Num l) * (Num r) = Num (l * r)
    l * r = errorBinary "(*)" l r

    signum (Num a) = Num $ signum a
    signum a = errorUnary "signum" a

    abs (Num a) = Num $ abs a
    abs a = errorUnary "abs" a

    negate (Num a) = Num (-a)
    negate a = errorUnary "negate" a

    fromInteger = Num

instance Enum Dyn where
    toEnum n = Num $ toInteger n

    fromEnum (Num a) = fromIntegral a
    fromEnum a = errorUnary "fromEnum" a 

instance Real Dyn where
    toRational (Num a) = toRational a
    toRational a = errorUnary "toRational" a 

instance Integral Dyn where
    toInteger (Num a) = a
    toInteger a = errorUnary "toInteger" a

    quotRem (Num l) (Num r) = let (quot', rem') = quotRem l r in (Num quot', Num rem')
    quotRem l r = errorBinary "quotRem" l r

apply :: Dyn -> Dyn -> Dyn
apply (Fun f) = f
apply _ = error "This value is not a function"

(!!!) :: Dyn -> Int -> Dyn
(List a) !!! n = a !! n
(Str a) !!! n = Str [a !! n]
other !!! n = error $ "Indexing is supported for " ++ showType other

(+++) :: Dyn -> Dyn -> Dyn
(Str l) +++ (Str r) = Str (l ++ r)
(List l) +++ (List r) = List (l ++ r)
(List l) +++ r = List (l ++ [r])
l +++ (List r) = List (l : r)
a +++ b = errorBinary "(+++)" a b

atoi :: Dyn -> Dyn
atoi (Str a) = Num $ read a
atoi a = errorUnary "atoi" a

itoa :: Dyn -> Dyn
itoa (Num a) = Str $ show a
itoa a = errorUnary "itoa" a

someDyn = List [Str "Hey!", List [Num 2 * Num 3, Fun (\Void -> Str "Wow")], Void]
testDyn = apply (someDyn !!! 1 !!! 1) Void +++ itoa (Num 42)

-- Комбинаторы

k :: Dyn
k = Fun (\x -> Fun (\_ -> x))

s :: Dyn -> Dyn -> Dyn -> Dyn
s x y z = apply (apply x z) (apply y z)

i :: Dyn
i = Fun (s k k)

testI = apply i (Num 42) -- Выводит 42, что и ожидается (следовательно, комбинаторы s и k работают)

-- Комбинатор M = S(I,I)
m :: Dyn -> Dyn
m = s i i

-- Проверка M-комбинатора основана на том факте, что M x = x x (из лекции 1), откуда M I x = I I x = I x = x
testM = apply (m i) (Str "Hey") -- Выводит "Hey", что подтверждает работоспособность M-комбинатора
-- На всякий случай доказательство того, что II x = x:
testII = apply (apply i i) (List [Num 10, Str "Ten"])