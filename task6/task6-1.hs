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

(!!) :: DList a -> Int -> a
DEmpty !! _ = error "Empty list"
dlist !! idx = if idx < 0 then error "Negative index" else dlist `index` idx where
    index :: DList a -> Int -> a
    index (DCons _ val _) 0 = val
    index (DCons _ _ DEmpty) _ = error "Index out of bounds"
    index (DCons _ _ right) cnt = right `index` pred cnt

insert :: DList a -> Int -> a -> DList a
insert dlist idx elem = if idx < 0 then error "Negative index" else insert' dlist idx elem where
    insert' :: DList a -> Int -> a -> DList a
    insert' DEmpty 0 newval = DCons DEmpty newval DEmpty
    insert' DEmpty _ _ = error "Index out of bounds"
    insert' (DCons left val DEmpty) 0 newval = rec where
        rec = DCons left newval right
        right = DCons rec val DEmpty
    insert' (DCons left val (DCons _ rh rt)) 0 newval = rec where
        rec = DCons left newval newright
        newright = DCons rec val (DCons newright rh rt)
    insert' (DCons left val right) cnt newval = DCons left val $ insert' right (pred cnt) newval
    
delete :: DList a -> Int -> DList a
delete DEmpty _ = error "Empty list"
delete dlist idx = if idx < 0 then error "Negative index" else delete' dlist idx where
    delete' :: DList a -> Int -> DList a
    delete' (DCons _ _ DEmpty) 0 = DEmpty
    delete' (DCons _ _ DEmpty) _ = error "Index out of bounds"
    delete' (DCons left _ (DCons _ rh rt)) 0 = DCons left rh rt 
    delete' (DCons left val right) cnt = DCons left val $ delete' right (pred cnt)