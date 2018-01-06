import Data.Stream

monom :: Double -> Integer -> Double
monom x n = sign * product (fmap ((x/) . fromInteger) [1..(2*n - 1)]) where
    sign = if even (n - 1) then 1 else (-1)

sin' :: Double -> Integer -> Double
sin' x n = sum $ fmap (monom x) [1..n]

sinPrecisions :: Double -> Stream Double
sinPrecisions x = sinPrecisions' x 1 where
    sinPrecisions' x n = sin' x n <:> sinPrecisions' x (n+1)

test = Data.Stream.take 10 $ sinPrecisions (pi/2)