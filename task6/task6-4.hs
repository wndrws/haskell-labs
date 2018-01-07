import Data.List (sort)

data BinaryTree a = 
      EmptyBinaryTree (BinaryTree a)
    | Leaf a (BinaryTree a)
    | Node a (BinaryTree a) (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
    show (EmptyBinaryTree _) = "<>"
    show (Leaf a _) = "L<" ++ show a ++ ">"
    show (Node a left right _) = "(" ++ show left ++ " N<" ++ show a ++ "> " ++ show right ++ ")"

instance (Eq a) => Eq (BinaryTree a) where
    (EmptyBinaryTree _) == (EmptyBinaryTree _) = True
    (EmptyBinaryTree _) == _ = False
    _ == (EmptyBinaryTree _) = False
    Leaf a _ == Leaf b _ = a == b
    Node a al ar _ == Node b bl br _ = a == b && al == bl && ar == br
    _ == _ = False

-- Создание пустого дерева
emptyTree :: (Ord a) => BinaryTree a
emptyTree = EmptyBinaryTree emptyTree

-- Создание дерева из списка
-- (позволяет получить сбалансированное дерево ценой предварительной сортировки списка)
treeFromList :: (Ord a) => [a] -> BinaryTree a
treeFromList list = treeFromList' list emptyTree where
    treeFromList' [] _ = emptyTree
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
insert (EmptyBinaryTree parent) val = let self = Leaf val (parent `updChild` self) in self
insert src@(Leaf val parent) newval
    | newval < val = let self = Node val (Leaf newval self) emptyTree (parent `updChild` self) in self
    | newval > val = let self = Node val emptyTree (Leaf newval self) (parent `updChild` self) in self
    | newval == val = src
insert src@(Node val left right parent) newval
    | newval < val = let self = Node val (insert left newval) right (parent `updChild` self) in self
    | newval > val = let self = Node val left (insert right newval) (parent `updChild` self) in self
    | newval == val = src

-- Удаление элемента
remove :: (Ord a) => BinaryTree a -> a -> BinaryTree a
remove src@(EmptyBinaryTree _) _ = src
remove src@(Leaf val _) valToRemove = if val == valToRemove then emptyTree else src
remove (Node val left right parent) valToRemove
    | valToRemove < val = makeLeafs $ let self = Node val (remove left valToRemove) right (parent `updChild` self) in self
    | valToRemove > val = makeLeafs $ let self = Node val left (remove right valToRemove) (parent `updChild` self) in self
    | valToRemove == val = makeLeafs $ if right == emptyTree then left `chParent` parent
        else node where
            node = Node newval (left `chParent` node) newright (parent `updChild` node)
            newval = findLeftmostVal right
            newright = remove right newval `chParent` node

chParent :: (Eq a) => BinaryTree a -> BinaryTree a -> BinaryTree a
chParent src@(EmptyBinaryTree _) _ = src
chParent (Leaf val _) parent = let self = Leaf val (parent `updChild` self) in self
chParent (Node val left right _) parent = let self = Node val left right (parent `updChild` self) in self

updChild :: (Eq a) => BinaryTree a -> BinaryTree a -> BinaryTree a
updChild (Node val left right parent) child
    | child == left = let self = Node val child right (parent `updChild` self) in self
    | child == right = let self = Node val left child (parent `updChild` self) in self
    | otherwise = error "Wrong child provided"
updChild tree _ = tree

findLeftmostVal :: (Ord a) => BinaryTree a -> a
findLeftmostVal (Leaf val _) = val
findLeftmostVal (Node val (EmptyBinaryTree _) _ _) = val
findLeftmostVal (Node _ left _ _) = findLeftmostVal left

makeLeafs :: (Ord a) => BinaryTree a -> BinaryTree a
makeLeafs src@(EmptyBinaryTree _) = src
makeLeafs src@(Leaf _ _) = src
makeLeafs (Node val left right parent)
    | left == emptyTree && right == emptyTree = Leaf val parent
    | otherwise = Node val (makeLeafs left) (makeLeafs right) parent

-- Поиск элемента в дереве
containsElement :: (Ord a) => BinaryTree a -> a -> Bool
containsElement (EmptyBinaryTree _) _ = False
containsElement (Leaf val _) target = val == target
containsElement (Node val left right _) target
    | target < val = containsElement left target
    | target > val = containsElement right target
    | target == val = True