data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

instance Show WeirdPeanoNumber where
    show Zero = "{0}"
    show wpn = "{" ++ show (toInt wpn) ++ "}"

toInt :: WeirdPeanoNumber -> Int
toInt Zero = 0
toInt (Succ a) = toInt a + 1
toInt (Pred a) = toInt a - 1

fromInt :: Int -> WeirdPeanoNumber
fromInt x | x > 0 = Succ $ fromInt (x - 1)
          | x < 0 = Pred $ fromInt (x + 1)
          | otherwise = Zero

instance Eq WeirdPeanoNumber where
    left == right = isEqualNormalized (normalize left) (normalize right) where
        isEqualNormalized :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
        isEqualNormalized Zero Zero = True
        isEqualNormalized Zero _ = False
        isEqualNormalized _ Zero = False
        isEqualNormalized (Succ a) (Succ b) = isEqualNormalized a b
        isEqualNormalized (Pred a) (Pred b) = isEqualNormalized a b
        isEqualNormalized _ _ = False

-- Нужна функция для взаимоуничтожения Succ'ов и Pred'ов, которые компенсируют друг друга
normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize Zero = Zero
normalize (Succ (Pred a)) = normalize a
normalize (Pred (Succ a)) = normalize a
normalize (Succ a) = let aNorm = normalize a in 
    case aNorm of
        (Pred b) -> b
        _ -> Succ aNorm
normalize (Pred a) = let aNorm = normalize a in 
    case aNorm of
        (Succ b) -> b
        _ -> Pred aNorm

instance Ord WeirdPeanoNumber where
    compare left right = compareNormalized (normalize left) (normalize right) where
        compareNormalized :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
        compareNormalized Zero other = case other of
            Zero -> EQ
            (Succ _) -> LT
            (Pred _)  -> GT
        compareNormalized (Pred a) other = case other of
            (Pred b)  -> compareNormalized a b
            _ -> LT
        compareNormalized (Succ a) other = case other of
            (Succ b)  -> compareNormalized a b
            _ -> GT