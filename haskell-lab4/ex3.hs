data BinTree a = EmptyTree |
               Node a (BinTree a) (BinTree a) 
               deriving Show
 
sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyTree = 0
sumBinTree Node x left right = x + (sumBinTree left) + (sumBinTree right)   


depthOfBT :: BinTree a -> Int
depthOfBT EmptyTree = 0
depthOfBT (Node x left right) = depth 1 left right
                                where 
                                   depth acc EmptyTree EmptyTree = acc
                                   depth acc (Node x left right) EmptyTree = max (depth (acc+1) left EmptyTree) (depth (acc+1) EmptyTree right)
                                   depth acc EmptyTree (Node x left right) = max (depth (acc+1) left EmptyTree) (depth (acc+1) EmptyTree right)
                                   depth acc left right = max (depth (acc) left EmptyTree) (depth (acc) EmptyTree right)


inOrder :: BinTree a -> [a]
inOrder EmptyTree = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right  

preOrder :: BinTree a -> [a]
preOrder EmptyTree = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right  

postOrder :: BinTree a -> [a]
postOrder EmptyTree = []
postOrder (Node x left right) = postOrder left ++ postOrder right  ++ [x]


mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyTree = EmptyTree
mapBT f (Node x left right) = (Node (f x) (mapBT f left) (mapBT f right)) 

singleton :: a -> BinTree a
singleton x = Node x EmptyTree EmptyTree


insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyTree = singleton x
insert x (Node current left right)
       | x < current = Node current (insert x left) right
       | x > current = Node current left (insert x right)
       | x == current = Node current left right

occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyTree = 0
occurs x (Node current left right) | x == current = 1 + (occurs x left) + (occurs x right)
                                   | otherwise = (occurs x left) + (occurs x right)


elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyTree = False
elemOf x (Node current left right) | x == current = True
                                   | otherwise = (elemOf x left) || (elemOf x right)

reflect :: BinTree a -> BinTree a
reflect EmptyTree = EmptyTree
reflect (Node x left right) = (Node x (reflect right) (reflect left))

countTree :: BinTree a -> Int
countTree EmptyTree = 0
countTree (Node x left right) = 1 + countTree left + countTree right








