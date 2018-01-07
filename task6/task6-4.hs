import Data.List (sort)

data BinaryTree a = 
      EmptyBinaryTree
    | Leaf a (BinaryTree a)
    | Node a (BinaryTree a) (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
    show EmptyBinaryTree = "<>"
    show (Leaf a _) = "L<" ++ show a ++ ">"
    show (Node a left right _) = "(" ++ show left ++ " N<" ++ show a ++ "> " ++ show right ++ ")"

instance (Eq a) => Eq (BinaryTree a) where
    EmptyBinaryTree == EmptyBinaryTree = True
    EmptyBinaryTree == _ = False
    _ == EmptyBinaryTree = False
    Leaf a _ == Leaf b _ = a == b
    Node a al ar _ == Node b bl br _ = a == b && al == bl && ar == br
    _ == _ = False

-- Создание пустого дерева
emptyTree :: (Ord a) => BinaryTree a
emptyTree = EmptyBinaryTree

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

-- Удаление элемента
remove :: (Ord a) => BinaryTree a -> a -> BinaryTree a
remove EmptyBinaryTree _ = EmptyBinaryTree
remove src@(Leaf val _) valToRemove = if val == valToRemove then EmptyBinaryTree else src
remove (Node val left right parent) valToRemove
    | valToRemove < val = makeLeafs $ Node val (remove left valToRemove) right parent
    | valToRemove > val = makeLeafs $ Node val left (remove right valToRemove) parent
    | valToRemove == val = makeLeafs $ if right == EmptyBinaryTree then left `chParent` parent
        else node where
            node = Node newval (left `chParent` node) newright parent
            newval = findLeftmostVal right
            newright = remove right newval `chParent` node

chParent :: BinaryTree a -> BinaryTree a -> BinaryTree a
chParent EmptyBinaryTree _ = EmptyBinaryTree
chParent (Leaf val _) parent = Leaf val parent
chParent (Node val left right _) parent = Node val left right parent

findLeftmostVal :: (Ord a) => BinaryTree a -> a
findLeftmostVal (Leaf val _) = val
findLeftmostVal (Node val EmptyBinaryTree _ _) = val
findLeftmostVal (Node _ left _ _) = findLeftmostVal left

makeLeafs :: (Ord a) => BinaryTree a -> BinaryTree a
makeLeafs EmptyBinaryTree = EmptyBinaryTree
makeLeafs src@(Leaf _ _) = src
makeLeafs (Node val left right parent)
    | left == EmptyBinaryTree && right == EmptyBinaryTree = Leaf val parent
    | otherwise = Node val (makeLeafs left) (makeLeafs right) parent

-- Поиск элемента в дереве
containsElement :: (Ord a) => BinaryTree a -> a -> Bool
containsElement EmptyBinaryTree _ = False
containsElement (Leaf val _) target = val == target
containsElement (Node val left right _) target
    | target < val = containsElement left target
    | target > val = containsElement right target
    | target == val = True