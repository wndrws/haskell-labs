data DList a = DEmpty | DCons (DList a) a (DList a)

instance (Eq a) => Eq (DList a) where
    DEmpty == DEmpty = True
    (DCons _ lh lt) == (DCons _ rh rt) = (lh == rh) && (lt == rt)
  
instance (Show a) => Show (DList a) where
    show lst = "[" ++ show' lst ++ "]"
        where
        show' DEmpty = ""
        show' (DCons _ h DEmpty) = show h
        show' (DCons _ h t) = show h ++ ", " ++ show' t

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DEmpty lst where
    list2dlist' :: DList a -> [a] -> DList a
    list2dlist' _ [] = DEmpty
    list2dlist' left (h:t) = rec where
        rec = DCons left h (list2dlist' rec t)

