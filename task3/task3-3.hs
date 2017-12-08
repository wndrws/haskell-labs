newtype PSet a = PSet { contains :: a -> Bool }

newtype PSetOr a = PSetOr { containsPSetOr :: a -> Bool }

newtype PSetAnd a = PSetAnd { containsPSetAnd :: a -> Bool }

newtype PSetXor a = PSetXor { containsPSetXor :: a -> Bool }

newtype PSetLeftNotRight a = PSetLeftNotRight { containsPSetLeftNotRight :: a -> Bool }

newtype PSetRightNotLeft a = PSetRightNotLeft { containsPSetRightNotLeft :: a -> Bool }

newtype PSetLeft a = PSetLeft { containsPSetLeft :: a -> Bool }

newtype PSetRight a = PSetRight { containsPSetRight :: a -> Bool }

-- Для самой структуры PSet не делаем никакой реализации класса Monoid, 
-- так как нельзя однозначно определить подходящий по умолчанию способ

instance Monoid (PSetOr a) where -- Объединение множеств по ИЛИ
    mempty = PSetOr (const False)
    mappend (PSetOr p1) (PSetOr p2) = PSetOr (\a -> p1 a || p2 a)
    
instance Monoid (PSetAnd a) where -- Объединение множество по И
    mempty = PSetAnd (const True)
    mappend (PSetAnd p1) (PSetAnd p2) = PSetAnd (\a -> p1 a && p2 a)

instance Monoid (PSetXor a) where -- Объединение множеств по Исключающему ИЛИ
    mempty = PSetXor (const False)
    mappend (PSetXor p1) (PSetXor p2) = PSetXor (\a -> p1 a && not(p2 a) || not(p1 a) && p2 a)

instance Monoid (PSetLeftNotRight a) where -- Левое множество минус правое (неассоциативный моноид)
    mempty = PSetLeftNotRight (const False)
    mappend (PSetLeftNotRight p1) (PSetLeftNotRight p2) = PSetLeftNotRight (\a -> p1 a && not(p2 a))

instance Monoid (PSetRightNotLeft a) where -- Правое множество минус левое (неассоциативный моноид)
    mempty = PSetRightNotLeft (const False)
    mappend (PSetRightNotLeft p1) (PSetRightNotLeft p2) = PSetRightNotLeft (\a -> not(p1 a) && p2 a)

instance Monoid (PSetLeft a) where -- Игнорировать правое множество при объединении (неассоциативный моноид)
    mempty = PSetLeft (const False)
    mappend left _ = left

instance Monoid (PSetRight a) where -- Игнорировать левое множество при объединении (неассоциативный моноид)
    mempty = PSetRight (const False)
    mappend _ right = right

-- Ниже приведена идея реализации класса Functor:
-- Суть в том, чтобы наложить ограничения на типы в сигнатуре фунции fmap:
--                       fmap :: (a -> b) -> f a -> f b 
-- Нужно заставить Хаскелль понять, что a и b здесь - это функции, возвращающие Bool, а не произвольные типы.
-- Как это сделать, я не знаю. Ниже приведена попытка, но она не компилируется.

{-
class Boolean a where
    test :: Bool

newtype Predicate p = Predicate { eval :: p -> Bool }

instance Boolean (Predicate p) where
    test = eval

newtype PSet' a = PSet' { contains' :: Predicate a }

instance Functor PSet' where
    fmap :: (Boolean a, Boolean b) => (a -> b) -> f a -> f b 
    fmap f (PSet p) = PSet $ f p
-}