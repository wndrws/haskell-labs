import Data.List (sort)

-- Бинарное дерево поиска для упорядочиваемых типов данных (в т.ч. Integer)
data BinaryTree a = 
      EmptyBinaryTree
    | Leaf a
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq)

-- Создание пустого дерева
emptyTree :: (Ord a) => BinaryTree a
emptyTree = EmptyBinaryTree

-- Добавление элемента
insert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insert EmptyBinaryTree val = Leaf val
insert src@(Leaf val) newval
    | newval < val = Node val (Leaf newval) EmptyBinaryTree
    | newval > val = Node val EmptyBinaryTree (Leaf newval)
    | newval == val = src
insert src@(Node val left right) newval
    | newval < val = Node val (insert left newval) right
    | newval > val = Node val left (insert right newval)
    | newval == val = src

-- Удаление элемента
remove :: (Ord a) => BinaryTree a -> a -> BinaryTree a
remove EmptyBinaryTree _ = EmptyBinaryTree
remove src@(Leaf val) valToRemove = 
    if val == valToRemove then EmptyBinaryTree else src
remove src@(Node val left right) valToRemove
    | valToRemove < val = makeLeafs $ Node val (remove left valToRemove) right
    | valToRemove > val = makeLeafs $ Node val left (remove right valToRemove)
    | valToRemove == val = makeLeafs $ 
        if right == EmptyBinaryTree then left 
            else Node newval left newright where
                newval = findLeftmostVal right
                newright = remove right newval

findLeftmostVal :: (Ord a) => BinaryTree a -> a
findLeftmostVal (Leaf val) = val
findLeftmostVal (Node val EmptyBinaryTree _) = val
findLeftmostVal (Node val left _) = findLeftmostVal left

makeLeafs :: (Ord a) => BinaryTree a -> BinaryTree a
makeLeafs EmptyBinaryTree = EmptyBinaryTree
makeLeafs src@(Leaf _) = src
makeLeafs (Node val left right)
    | left == EmptyBinaryTree && right == EmptyBinaryTree = Leaf val
    | otherwise = Node val (makeLeafs left) (makeLeafs right)

-- Поиск элемента в дереве
containsElement :: (Ord a) => BinaryTree a -> a -> Bool
containsElement EmptyBinaryTree _ = False
containsElement (Leaf val) target = val == target
containsElement (Node val left right) target
    | target < val = containsElement left target
    | target > val = containsElement right target
    | target == val = True

-- Поиск в дереве наименьшего элемента, который больше или равен заданному
nearestGE :: (Ord a) => BinaryTree a -> a -> a
nearestGE EmptyBinaryTree _ = error "Can't search in empty tree!"
nearestGE (Leaf val) target = if val >= target then val else error "Not found"
nearestGE tree@(Node val _ _) target = findNearestGE tree target val where
    findNearestGE :: (Ord a) => BinaryTree a -> a -> a -> a
    findNearestGE EmptyBinaryTree target previousVal =
        if previousVal >= target then previousVal else error "Not found"
    findNearestGE (Leaf val) target previousVal
        | val >= target = val 
        | previousVal >= target = previousVal 
        | otherwise = error "Not found"
    findNearestGE (Node val left right) target previousVal
        | target == val = val
        | target < val = findNearestGE left target val
        | target > val = findNearestGE right target previousVal


-- Создание списка из дерева
listFromTree :: (Ord a) => BinaryTree a -> [a]
listFromTree EmptyBinaryTree = []
listFromTree (Leaf val) = [val]
listFromTree tree = leftmostVal : listFromTree newtree where
    leftmostVal = findLeftmostVal tree
    newtree = if isLeaf tree then tree else remove tree leftmostVal
    isLeaf :: (Ord a) => BinaryTree a -> Bool
    isLeaf (Leaf _) = True
    isLeaf _ = False

-- Создание дерева из списка
-- (позволяет получить сбалансированное дерево ценой предварительной сортировки списка)
treeFromList :: (Ord a) => [a] -> BinaryTree a
treeFromList [] = EmptyBinaryTree
treeFromList [a] = Leaf a
treeFromList unsortedList = 
    let lst = Data.List.sort unsortedList
        med = lst !! (length lst `div` 2)
        lp = fst $ halves lst
        rp = tail $ snd $ halves lst
        halves list = splitAt (length list `div` 2) list
    in Node med (treeFromList lp) (treeFromList rp)
