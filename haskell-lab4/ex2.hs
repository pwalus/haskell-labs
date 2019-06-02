type X = Int
type Y = Int
data CartInt2DVec = MkCartInt2DVec X Y


xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y



data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x



data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}







data List a = EmptyList | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyList = error "head': the empty list has no head!"
head' (Cons x xs) = x








data Coord3d a = Coord3d {getX::a, getY::a, getZ::a}



data Shape = Circle Float |
             Rectangle Float Float


area :: Shape -> Float
area (Circle r) = pi *(r ^ 2)
area (Rectangle x y) = x * y


data Tree a = EmptyTree |
              Node a (Tree a) (Tree a)
              deriving Show


rootValue :: Tree a -> a
rootValue (EmptyTree) = error "Empty Tree"
rootValue (Node x left right) = x





data TrafficLights = Green | Yellow | Red

actionFor :: TrafficLights -> String
actionFor Green = "run"
actionFor Yellow = "maybe stop"
actionFor Red = "stop"
