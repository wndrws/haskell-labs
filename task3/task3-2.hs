data ReverseList a = RNil | RCons (ReverseList a) a

instance (Eq a) => Eq (ReverseList a) where
    RNil == RNil = True
    RNil == _ = False
    _ == RNil = False
    (RCons lrlst a) == (RCons rrlst b) = a == b && lrlst == rrlst

instance (Show a) => Show (ReverseList a) where
    show RNil = "R[]"
    show (RCons RNil elem) = "R[" ++ show elem ++ "]"
    show rlist = "R[" ++ showRList rlist ++ "]" where 
        showRList (RCons RNil elem) = show elem
        showRList (RCons rlst elem) = showRList rlst ++ (',' : show elem)

instance (Ord a) => Ord (ReverseList a)  where
    RNil <= _ = True
    _ <= RNil = False
    (RCons lrlst a) <= (RCons rrlst b)
        | lrlst <= rrlst = True
        | otherwise = a <= b

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend = rconcat

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons rlst elem) = RCons (fmap f rlst) (f elem)

rconcat :: ReverseList a -> ReverseList a -> ReverseList a
rconcat RNil RNil = RNil
rconcat RNil rlst = rlst
rconcat rlst RNil = rlst
rconcat (RCons lrlst a) (RCons RNil b) = RCons (RCons lrlst a) b
rconcat (RCons RNil a) (RCons rrlst b) = RCons (attachLeft a rrlst) b where
    attachLeft :: a -> ReverseList a -> ReverseList a
    attachLeft elem RNil = RCons RNil elem
    attachLeft elem (RCons inner val) = RCons (attachLeft elem inner) val
rconcat lrlst (RCons rrlst rval) = RCons (rconcat lrlst rrlst) rval

fromList :: [a] -> ReverseList a
fromList list = fromReversedList $ reverse list where
    fromReversedList [] = RNil
    fromReversedList [elem] = RCons RNil elem 
    fromReversedList (h:t) = RCons (fromReversedList t) h

toList :: ReverseList a -> [a]
toList rlist = reverse $ toReversedList rlist where
    toReversedList RNil = []
    toReversedList (RCons RNil elem) = [elem]
    toReversedList (RCons rlst elem) = elem : toReversedList rlst