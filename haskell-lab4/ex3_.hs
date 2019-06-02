data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show


sumTree :: Num a => Tree a -> a 
sumTree Empty = 0
sumTree (Node x left right) = x + sumTree left + sumTree right


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree mapper (Node x left right) = Node (mapper x) (mapTree mapper left) (mapTree mapper right)


insert :: Ord a => a -> Tree a -> Tree a
insert value Empty = (Node value Empty Empty)
insert value (Node x left right) | value < x = Node x (insert value left) right
                                 | value > x = Node x left (insert value right)
                                 | otherwise = Node x left right




data TrafficLight = TrafficLight {red::String, green::String, yellow::String} deriving Show


data Light = Light String String deriving Show


instance Eq a => Eq (Tree a) where 
   (==) (Node x left right) (Node y lefty righty) = x == y


instance Eq TrafficLight where
  (==) left right = red left == red right 

instance Eq Light where
  (==) (Light leftx rightx) (Light lefty righty) = leftx == lefty
