import Data.List (sort)

data BinaryTree a = 
      EmptyBinaryTree
    | Leaf a (BinaryTree a)
    | Node a (BinaryTree a) (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
    show EmptyBinaryTree = "<>"
    show (Leaf a _) = "L<" ++ show a ++ ">"
    show (Node a left right _) = "(" ++ show left ++ " N<" ++ show a ++ "> " ++ show right ++ ")"

-- Создание пустого дерева
emptyTree :: (Ord a) => BinaryTree a
emptyTree = EmptyBinaryTree

-- Добавление элемента
insert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insert EmptyBinaryTree val = Leaf val EmptyBinaryTree
insert src@(Leaf val parent) newval
    | newval < val = let self = Node val (Leaf newval self) EmptyBinaryTree parent in self
    | newval > val = let self = Node val EmptyBinaryTree (Leaf newval self) parent in self
    | newval == val = src
insert src@(Node val left right parent) newval
    | newval < val = let self = Node val (insert left newval) right parent in self
    | newval > val = let self = Node val left (insert right newval) parent in self
    | newval == val = src

-- Создание дерева из списка
-- (позволяет получить сбалансированное дерево ценой предварительной сортировки списка)
treeFromList :: (Ord a) => [a] -> BinaryTree a
treeFromList list = treeFromList' list EmptyBinaryTree where
    treeFromList' [] _ = EmptyBinaryTree
    treeFromList' [a] parent = Leaf a parent
    treeFromList' unsortedList parent = 
        let lst = Data.List.sort unsortedList
            med = lst !! (length lst `div` 2)
            lp = fst $ halves lst
            rp = tail $ snd $ halves lst
            halves list = splitAt (length list `div` 2) list
            node = Node med (treeFromList' lp node) (treeFromList' rp node) parent
        in node