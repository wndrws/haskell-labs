import Data.Stream as S
import Data.Ratio

-- Представление бесконечной цепной дроби для числа e в виде потока
eChain :: Stream Integer
eChain = 2 <:> eChainCycle 2 where
    eChainCycle n = Cons 1 (Cons n (Cons 1 (eChainCycle $ n+2)))

-- n-я подходящая дробь для цепной дроби числа e (по рекуррентным формулам Эйлера)
eFrac :: Int -> Rational
eFrac n = eNumers n % eDenoms n where
    eNumers :: Int -> Integer
    eNumers (-1) = 1
    eNumers 0 = S.head eChain
    eNumers n = (eChain S.!! n) * eNumers (n-1) + eNumers (n-2)
    eDenoms :: Int -> Integer
    eDenoms (-1) = 0
    eDenoms 0 = 1
    eDenoms n = (eChain S.!! n) * eDenoms (n-1) + eDenoms (n-2)

ePrecisions :: Stream Rational
ePrecisions = eFrac 0 <:> S.fromList (fmap eFrac [1..])

test = S.take 20 ePrecisions -- Последний выведенный элемент отличается от e менее чем на 10^-15