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